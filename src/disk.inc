;debug=1
;
; FDC Driver for Intel 82077(A/AA), PC8477BV and compatibles
;

.pc02

FDCBASE = $D300
FDCSRA  = FDCBASE
FDCSRB  = FDCBASE+1
FDCDOR  = FDCBASE+2
FDCTDR  = FDCBASE+3
FDCMSR  = FDCBASE+4
FDCDSR  = FDCBASE+4
FDCFIFO = FDCBASE+5
FDCDIR  = FDCBASE+7
FDCCCR  = FDCBASE+7


; FDC Commands
;
.scope FDC
SPECIFY      = $03
WRITE        = $45
READ         = $46
RECALIBRATE  = $07
SENSE        = $08
FORMATTRACK  = $0D
SEEK         = $0F
VERSION      = $10
CONFIGURE    = $13
LOCK         = $14
READID       = $4A
.endscope

;
; Check MSR and loop until it returns the desired value
; Carry will be set on error/timeout
CHECKMSR_C: STZ IOTMR
@LOOP:      delay $40
            LDA IOTMR
            CMP #$03
            BCC @OK
            LDA #ERR::EIO
            SEC
            RTS
@OK:        LDA FDCMSR
            AND #$F0
            CMP #$80
            BNE @LOOP
            CLC
            RTS

CHECKMSR_R: STZ IOTMR
@LOOP:      delay $40
            LDA IOTMR
            CMP #$03
            BCC @OK
            LDA #ERR::EIO
            SEC
            RTS
@OK:        LDA FDCMSR
            AND #$F0
            CMP #$D0
            BNE @LOOP
            CLC
            RTS

CHECKMSR_W: STZ IOTMR
@LOOP:      delay $40
            LDA IOTMR
            CMP #$03
            BCC @OK
            LDA #ERR::EIO
            SEC
            RTS
@OK:        LDA FDCMSR
            AND #$F0
            CMP #$90
            BNE @LOOP
            CLC
            RTS

FDCCOMMAND: PHA
            JSR CHECKMSR_C
            BCS @ERR
            PLA
            STA FDCFIFO
@END:       RTS
@ERR:       PLA
            LDA #ERR::EIO
            SEC
            RTS

; FDCINIT
;
; Initialize the FDC
;
FDCINIT:    STZ FDCDRVSEL
            JSR FDCRST
            BCC @RSTDONE
@ERROR:     SEC
            RTS
@RSTDONE:   LDX #04                    ; Issue 4 sense interrupt commands after reset, 1 for each possible drive
@L_SNS:     JSR FDCSNS
            BCS @ERROR
            DEX
            BNE @L_SNS
            STZ FDCCCR                 ; Set Density in CCR register to 500kbit/s
            LDA #FDC::CONFIGURE
            JSR FDCCOMMAND
            BCS @ERROR
            JSR CHECKMSR_W
            BCS @ERROR
            STZ FDCFIFO
            JSR CHECKMSR_W
            BCS @ERROR
            LDA #%00001111             ; Implied seeks disabled, FIFO Enabled, Polling enabled, Fifo depth 16
            STA FDCFIFO
            JSR CHECKMSR_W
            BCS @ERROR
            STZ FDCFIFO                ; Precompensation from track 0 (default)
            LDA #FDC::SPECIFY
            JSR FDCCOMMAND
            BCS @ERROR
            JSR CHECKMSR_W
            BCS @ERROR
            LDA #$80                   ; Step rate: 8ms, Head unload time: MAX
            STA FDCFIFO
            JSR CHECKMSR_W
            BCS @ERROR
            LDA #$A1                   ; Head Load time = 10ms, disable DMA
            STA FDCFIFO
            STZ FDCDRV
            JSR FDCSELECT              ; Select drive A
            LDA #01
            JSR FDCSEEK                ; Seek to Track 1
            BCS @ERROR
            LDA #34
            JSR FDCSEEK                ; Seek to Track 34
            BCS @ERROR
            JSR FDCRECAL               ; Recalibrate
            BCS @END
            STZ SECTORBUFPTR
            LDA #$10
            STA SECTORBUFPTR+1
@END:       RTS

LBACHS:     
            m_copy16 NEXTSECTOR, MR_2  ; CYL = LBA/(Heads * Sectors per track)
            STZ MR_1
            STZ MR_1+1
            LDA #36                    ; (Heads * Sectors per track) hardcoded to 36 for 1.44MB Floppy
            STA MR_1
            JSR divide
            LDA MR_2                   ; Get the result
            STA LBACYL                 ; And save it
            STZ MR_1
            STZ MR_1+1
            STZ MR_2
            STZ MR_2+1
            LDA MR_3+1
            LDA MR_3

            m_copy16 MR_3, MR_2        ; HEAD = (LBA % (Heads * Sectors per track)) / Sectors per track
            LDA #18                    ; LBA % (Heads * Sectors per track) already in MR_3 as remainder of previous division
            STA MR_1                   ; Sectors per track
            JSR divide
            LDA MR_2                   ; Get the result
            STA LBAHEAD                ; And save it

            LDA MR_3
            INC                        ; SECTOR = (LBA % (Heads * Sectors per track)) % SPT + 1
            STA LBASECT                ; Already calculated as remainder of HEAD calculation
            RTS


; FDC Reset
; Reset the FDC and wait for it to report ready
; Carry will be set if the reset times out
;
FDCRST:     debugmsg "Resetting FDC"
            STZ FDCDOR
            delay $ff
            LDA #$04
            STA FDCDOR
            LDX #$FF
@WAIT:      LDA FDCMSR
            CMP #$80
            BEQ @DONE
            DEX
            BNE @WAIT
            LDA #ERR::EIO
            SEC
            RTS
@DONE:      CLC
            RTS

; FDCREADLBA
; Convert SECTOR (in NEXTSECTOR) to C/H/S then fall through to FDCREAD
;
FDCREADLBA: debugmsg "Sector: ", NEXTSECTOR+1, NEXTSECTOR
            JSR LBACHS
            LDA LBASECT
            LDX LBACYL
            LDY LBAHEAD

; READ
; A: Sector
; X: Cylinder
; Y: Head
; Reads a sector into (SECTORBUFPTR)
FDCREAD:    PHX
            PHA
            CPX #80                    ; Don't seek past end of disk
            BCS @SEEKERR
            JSR FDCSELECT
@CHECK:     LDA FDCDIR
            BPL @GO
            LDA #$01
            JSR FDCSEEK
            BCS @SEEKERR
            LDA #$00
            JSR FDCSEEK
            BCS @SEEKERR
            LDA FDCDIR
            BPL @GO
            PLA
            PLX
            LDA #ERR::ENODISK
            SEC
            RTS
@SEEKERR:   PLA
            PLX
            LDA #ERR::EIO
            RTS
@GO:        TXA
            JSR FDCSEEK
            BCS @SEEKERR
            LDA #FDC::READ
            JSR FDCCOMMAND
            BCC @OK
@ERR2:      PLA
            PLX
            RTS
@OK:        JSR CHECKMSR_W
            BCC @RDY
            JMP @ERR
@RDY:       TYA                        ; Set head and drive number
            AND #$01                   ;
            ASL                        ;
            ASL                        ;
            ORA FDCDRV                 ;
            STA FDCFIFO                ;
            JSR CHECKMSR_W
            BCC @TRACK
            JMP @ERR
@TRACK:     TXA                        ; Set track number
            STA FDCFIFO                ;
            JSR CHECKMSR_W
            BCS @ERR
            TYA                        ; Set head number (again...)
            AND #$01                   ;
            STA FDCFIFO                ;
            JSR CHECKMSR_W
            BCS @ERR
            PLA                        ; Set sector number
            PHA                        ;
            STA FDCFIFO                ;
            JSR CHECKMSR_W
            BCS @ERR
            LDA #$02                   ; Set bytes per sector (512)
            STA FDCFIFO                ;
            JSR CHECKMSR_W
            BCS @ERR
            PLA                        ; Set last sector number
            STA FDCFIFO                ;
            JSR CHECKMSR_W
            BCS @ERR+1
            LDA #$1B                   ; Set intersector gap length
            STA FDCFIFO                ;
            JSR CHECKMSR_W
            BCS @ERR+1
            LDA #$FF                   ; Set datalength
            STA FDCFIFO                ;
            LDX #$00
@READ:      delay $05
            LDA FDCMSR
            AND #$F0
            CMP #$F0
            BEQ @DATA
            CMP #$D0
            BEQ @STATUS
            CMP #$80
            BEQ @DONE
            LDA IOTMR
            CMP #$02
            BNE @READ
            BRA @ERR+1
@DATA:      LDY #$00
            LDA FDCFIFO
            STA (SECTORBUFPTR),Y
            INC SECTORBUFPTR
            BNE @READ
            INC SECTORBUFPTR+1
            STZ IOTMR
            BRA @READ
@STATUS:    LDA FDCFIFO
            STA FDCSR0,X
            INX
            BRA @READ
@DONE:      PLX
            LDA FDCSR1
            CMP #$80
            BNE @ERR+2
            CLC
@END:       RTS
@ERR:       PLA
            PLX
            LDA #ERR::EIO
            SEC
            RTS


; FDCREADLBA
; Convert SECTOR (in NEXTSECTOR) to C/H/S then fall through to FDCREAD
;
FDCWRITELBA: debugmsg "Sector: ", NEXTSECTOR+1, NEXTSECTOR
            JSR LBACHS
            LDA LBASECT
            LDX LBACYL
            LDY LBAHEAD

; READ
; A: Sector
; X: Cylinder
; Y: Head
; Reads a sector into (SECTORBUFPTR)
FDCWRITE:   PHX
            PHA
            CPX #80                    ; Don't seek past end of disk
            BCS @SEEKERR
            JSR FDCSELECT
@CHECK:     LDA FDCDIR
            BPL @GO
            LDA #$01
            JSR FDCSEEK
            BCS @SEEKERR
            LDA #$00
            JSR FDCSEEK
            BCS @SEEKERR
            LDA FDCDIR
            BPL @GO
            PLA
            PLX
            LDA #ERR::ENODISK
            SEC
            RTS
@SEEKERR:   PLA
            PLX
            LDA #ERR::EIO
            RTS
@GO:        TXA
            JSR FDCSEEK
            LDA #FDC::WRITE
            JSR FDCCOMMAND
            BCC @OK
            PLA
            PLX
            RTS
@OK:        JSR CHECKMSR_W
            BCC @RDY
            JMP @ERR
@RDY:       TYA                        ; Set head and drive number
            AND #$01                   ;
            ASL                        ;
            ASL                        ;
            ORA FDCDRV                 ;
            STA FDCFIFO                ;
            JSR CHECKMSR_W
            BCC @TRACK
            JMP @ERR
@TRACK:     TXA                        ; Set track number
            STA FDCFIFO                ;
            JSR CHECKMSR_W
            BCS @ERR
            TYA                        ; Set head number (again...)
            AND #$01                   ;
            STA FDCFIFO                ;
            JSR CHECKMSR_W
            BCS @ERR
            PLA                        ; Set sector number
            PHA                        ;
            STA FDCFIFO                ;
            JSR CHECKMSR_W
            BCS @ERR
            LDA #$02                   ; Set bytes per sector (512)
            STA FDCFIFO                ;
            JSR CHECKMSR_W
            BCS @ERR
            PLA                        ; Set last sector number
            STA FDCFIFO                ;
            JSR CHECKMSR_W
            BCS @ERR+1
            LDA #$1B                   ; Set intersector gap length
            STA FDCFIFO                ;
            JSR CHECKMSR_W
            BCS @ERR+1
            LDA #$FF                   ; Set datalength
            STA FDCFIFO                ;
            LDX #$00
@WRITE:     delay $05
            LDA FDCMSR
            AND #$F0
            CMP #$B0
            BEQ @DATA
            CMP #$D0
            BEQ @STATUS
            CMP #$80
            BEQ @DONE
            LDA IOTMR
            CMP #$02
            BNE @WRITE
            BRA @ERR+1
@DATA:      LDY #$00
            LDA (SECTORBUFPTR),Y
            STA FDCFIFO
            INC SECTORBUFPTR
            BNE @WRITE
            INC SECTORBUFPTR+1
            STZ IOTMR
            BRA @WRITE
@STATUS:    LDA FDCFIFO
            STA FDCSR0,X
            INX
            BRA @WRITE
@DONE:      PLX
            LDA FDCSR1
            CMP #$80
            BNE @ERR+2
            CLC
@END:       RTS
@ERR:       PLA
            PLX
            LDA #ERR::EIO
            SEC
            RTS

; READID
;
; Reads the first ID byte it can find on disk and reports back with cylinder, head, sector
;
FDCREADID:  PHX
            PHY
            LDY #$04
@TRY:       DEY
            BNE @GO
            LDA #ERR::EIO
            SEC
            BRA @ERROR
@GO:        LDA #FDC::READID
            BCS @DONE
            JSR CHECKMSR_W
            BCC @READY
            RTS
@READY:     LDA FDCDRV
            STA FDCFIFO
            LDX #$00
            JSR CHECKMSR_R
            BCS @ERROR
@LOOP:      LDA FDCFIFO
            STA FDCSR0,X
            INX
            LDA FDCMSR
            AND #$F0
            CMP #$D0
            BEQ @LOOP
            LDA FDCSR0
            AND #$F0
            BEQ @DONE
            BRA @TRY
@DONE:      CLC
@ERROR:     PLY
            PLX
            RTS

; RECALIBRATE
;
FDCRECAL:   LDA #FDC::RECALIBRATE
            JSR FDCCOMMAND
            BCS @END
            LDX #$00
            JSR CHECKMSR_W
            BCS @END
            STZ FDCFIFO
            JSR FDCSNS
            LDA FDCSR0
            AND #$D0
            BEQ @END
            LDA #ERR::EIO
            SEC
@END:       RTS

; SENSE INTERRUPT
; Sense interrupt codes from FDC and save them to FDDSR0 and FDDSR1
FDCSNS:     LDA #FDC::SENSE            ; Send SENSE command
            JSR FDCCOMMAND
            BCS @END
            delay $40
            LDA FDCMSR                 ; Sense interrupt will fail unless SEEK/RECAL is complete
            AND #$F0                   ; In this case MSR will be 0x80
            CMP #$80                   ;
            BEQ FDCSNS                 ; If it is, issue the sense again
@DATA:      JSR CHECKMSR_R             ; Wait for data
            BCS @END
            LDA FDCFIFO                ;
            STA FDCSR0                 ; Save SR0
            delay $40
            LDA FDCMSR                 ; Sense interrupt will fail unless SEEK/RECAL is complete
            AND #$F0                   ; In this case MSR will be 0x80
            CMP #$80                   ;
            BEQ FDCSNS                 ; If it is, issue the sense again
@RESULT:    JSR CHECKMSR_R             ; Get SR1
            BCS @END
            LDA FDCFIFO                ; Save SR1
            CLC
@END:       RTS

; Seek
;
; Seeks drive to cylinder specified by A
;
FDCSEEK:    PHA
            LDA #FDC::SEEK
            JSR FDCCOMMAND
            BCS @ERR
            JSR CHECKMSR_W
            BCC @READY
            PLA
            LDA #ERR::EIO
            SEC
            RTS
@READY:     LDA FDCDRV
            STA FDCFIFO
            JSR CHECKMSR_W
            BCS @ERR
            PLA
            BCC @GO
            RTS
@GO:        STA FDCFIFO
            JSR FDCSNS
            LDA FDCSR0
            AND #$D0
            BNE @ERR+1
            CLC
            RTS
@ERR:       PLA
            LDA #ERR::EIO
            SEC
            RTS

; Select drive and turn motor on
;
; Upper 4 bits are motor enable signals
; lower 2 bits are binary encoded drive number
;
FDCSELECT:  LDA FDCDRVSEL
            BEQ @GO
            RTS
@GO:        LDA #$FF
            STA FDCDRVSEL
            PHX
            LDA FDCDRV
            AND #$03
            TAX
            ORA FDCDOR
            STA FDCDOR
            INX
            TXA
            ASL A
            ASL A
            ASL A
            ASL A
            ORA FDCDOR
            STA FDCDOR
            LDA SECONDSL
            CLC
            ADC #01
@WAIT:      CMP SECONDSL
            BNE @WAIT
            PLX
            RTS

; Deselect all drives
;
FDCDSELECT: STZ FDCDRVSEL
            LDA FDCDOR
            AND #$04
            STA FDCDOR
            RTS