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

;;;;
FDCVARSTART = $0980
;;;;
NEXTSECTOR = FDCVARSTART
LBACYL     = NEXTSECTOR+2
LBAHEAD    = LBACYL+1
LBASECT    = LBAHEAD+1
FDCTRIES   = LBASECT+1
FDCSR0     = FDCTRIES+1
FDCSR1     = FDCSR0+1
FDCSR2     = FDCSR1+1
FDCTRN     = FDCSR2+1
FDCHDN     = FDCTRN+1
FDCSCN     = FDCHDN+1
FDCBPS     = FDCSCN+1
FDCEOT     = FDCBPS+1
FDCGPL     = FDCEOT+1
FDCDTL     = FDCGPL+1
FDCDRV     = FDCDTL+1
FDCDRVSEL  = FDCDRV+1
FDCSPT     = FDCDRVSEL+1 ; Sectors per track for selected disk
FDCRATEA   = FDCSPT+1    ; Data rate drive A (0=500Kbit/s, 2=250Kbit/s)
FDCRATEB   = FDCRATEA+1  ; Data rate drive B (0=500Kbit/s, 2=250Kbit/s)
;;;;
FDCVAREND  = FDCRATEB+1
;;;;
SECTORBUF  = $0A00


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
FDCINIT:    LDX #FDCVAREND-FDCVARSTART
            STZ FDCVARSTART,X
            DEX
            BNE FDCINIT+2
            STZ FDCVARSTART
            JSR FDCRST
            BCC @RSTDONE
@ERROR:     SEC
            RTS
@RSTDONE:   LDX #04               ; Issue 4 sense interrupt commands after reset, 1 for each possible drive
@L_SNS:     JSR FDCSNS
            BCS @ERROR
            DEX
            BNE @L_SNS
            STZ FDCCCR            ; Set Density in CCR register to 500kbit/s
            LDA #FDC::CONFIGURE
            JSR FDCCOMMAND
            BCS @ERROR
            JSR CHECKMSR_W
            BCS @ERROR
            STZ FDCFIFO
            JSR CHECKMSR_W
            BCS @ERROR
            LDA #%00001111        ; Implied seeks disabled, FIFO Enabled, Polling enabled, Fifo depth 16
            STA FDCFIFO
            JSR CHECKMSR_W
            BCS @ERROR
            STZ FDCFIFO           ; Precompensation from track 0 (default)
            LDA #FDC::SPECIFY
            JSR FDCCOMMAND
            BCS @ERROR
            JSR CHECKMSR_W
            BCS @ERROR
            LDA #$80              ; Step rate: 8ms, Head unload time: MAX
            STA FDCFIFO
            JSR CHECKMSR_W
            BCS @ERROR
            LDA #$A1              ; Head Load time = 10ms, disable DMA
            STA FDCFIFO
            STZ FDCDRV
            JSR FDCSELECT         ; Select drive A
            LDA #01
            JSR FDCSEEK           ; Seek to Track 1
            BCS @ERROR
            LDA #34
            JSR FDCSEEK           ; Seek to Track 34
            BCS @ERROR
            JSR FDCRECAL          ; Recalibrate
            BCS @END
            JSR FDCCHGCHK+5       ; Assume disk is changed and setup media type if a disk is present
            CLC                   ; Throw away error result from FDCCHGCHK
@END:       RTS

LBACHS:     
            m_copy16 NEXTSECTOR, MR_2  ; CYL = LBA/(Heads * Sectors per track)
            STZ MR_1
            STZ MR_1+1
            LDA #36                    ; (Heads * Sectors per track)
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

; CHANGE?

FDCCHGCHK:  LDA FDCDIR    ; CHANGING THESE LINES WILL BREAK THINGS
            BPL @NOCHANGE ; FDCINIT SKIPS TO FDCCHGCHK+5
            LDA #01
            JSR FDCSEEK
            LDA #$00
            JSR FDCSEEK
            LDA FDCDIR
            BMI @NODISK
            LDA #$00
            STA FDCCCR
            delay $ff
            ;JSR FDCREADID
            ;BCS @TRYDD
            LDX FDCDRV
            LDA #$00
            STA FDCRATEA,X
            CLC
            RTS
@TRYDD:     LDA #$00
            STA FDCCCR
            delay $ff
            delay $ff
            JSR FDCREADID
            BCS @ERROR
            LDA #$00
            LDX FDCDRV
            STA FDCRATEA,X
@NOCHANGE:  CLC
            RTS
@NODISK:    LDA #ERR::ENODISK
            SEC
            RTS
@ERROR:     BRK
            BRK
            LDA #ERR::EIO ; Didn't detect valid sector/track in both DD/HD rates
            RTS

; CONFIGDRV
; Set media type for the drive selected by FDCDRV
; Must be called after FDCSELECT & FDCCHGCHK
FDCCFGDRV:  LDX FDCDRV
            LDA FDCRATEA,X
            lda #$00
            STA FDCCCR
            BNE @RATE_DD
@RATE_HD:   LDA #36
            BRA @END
@RATE_DD:   LDA #18
@END:       STA FDCSPT
            RTS

; FDCREADLBA
; Convert SECTOR (in NEXTSECTOR) to C/H/S then fall through to FDCREAD
;
FDCREADLBA: debugmsg "Sector: ", NEXTSECTOR+1, NEXTSECTOR
            JSR FDCSELECT
            JSR FDCCHGCHK
            BCS @DONE
            JSR FDCCFGDRV
            LDA #$03
            STA FDCTRIES
@RETRY:     JSR LBACHS
            LDA LBASECT
            LDX LBACYL
            LDY LBAHEAD
            JSR FDCREAD
            BCC @DONE
            LDX FDCTRIES
            DEX
            STX FDCTRIES
            BEQ @DONE
            LDA #$40
            JSR FDCSEEK
            BRA @RETRY
@DONE:      RTS

; READ
; A: Sector
; X: Cylinder
; Y: Head
; Reads a sector into (SECTORBUFPTR)
FDCREAD:    PHX
            PHA
            CPX #80         ; Don't seek past end of disk
            LDA #ERR::EIO
            BCS @ERR2
@GO:        TXA
            JSR FDCSEEK
            LDA #ERR::EIO
            BCS @ERR2
            LDA #FDC::READ
            JSR FDCCOMMAND
            BCC @OK
@ERR2:      PLA
            PLX
            RTS
@OK:        JSR CHECKMSR_W
            BCC @RDY
            JMP @ERR
@RDY:       TYA                      ; Set head and drive number
            AND #$01                 ;
            ASL                      ;
            ASL                      ;
            ORA FDCDRV               ;
            STA FDCFIFO              ;
            JSR CHECKMSR_W
            BCC @TRACK
            JMP @ERR
@TRACK:     TXA                      ; Set track number
            STA FDCFIFO              ;
            JSR CHECKMSR_W
            BCS @ERR
            TYA                      ; Set head number (again...)
            AND #$01                 ;
            STA FDCFIFO              ;
            JSR CHECKMSR_W
            BCS @ERR
            PLA                      ; Set sector number
            PHA                      ;
            STA FDCFIFO              ;
            JSR CHECKMSR_W
            BCS @ERR
            LDA #$02                 ; Set bytes per sector (512)
            STA FDCFIFO              ;
            JSR CHECKMSR_W
            BCS @ERR
            PLA                      ; Set last sector number
            STA FDCFIFO              ;
            JSR CHECKMSR_W
            BCS @ERR+1
            LDA #$1B                 ; Set intersector gap length
            STA FDCFIFO              ;
            JSR CHECKMSR_W
            BCS @ERR+1
            LDA #$FF                 ; Set datalength
            STA FDCFIFO              ;
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
            RTS ; return with error code
FDCWRITELBA: debugmsg "Sector: ", NEXTSECTOR+1, NEXTSECTOR
            JSR FDCSELECT
            JSR FDCCHGCHK
            BCS FDCWRITELBA-1
            JSR FDCCFGDRV
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
            CPX #80         ; Don't seek past end of disk
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
@RDY:       TYA                      ; Set head and drive number
            AND #$01                 ;
            ASL                      ;
            ASL                      ;
            ORA FDCDRV               ;
            STA FDCFIFO              ;
            JSR CHECKMSR_W
            BCC @TRACK
            JMP @ERR
@TRACK:     TXA                      ; Set track number
            STA FDCFIFO              ;
            JSR CHECKMSR_W
            BCS @ERR
            TYA                      ; Set head number (again...)
            AND #$01                 ;
            STA FDCFIFO              ;
            JSR CHECKMSR_W
            BCS @ERR
            PLA                      ; Set sector number
            PHA                      ;
            STA FDCFIFO              ;
            JSR CHECKMSR_W
            BCS @ERR
            LDA #$02                 ; Set bytes per sector (512)
            STA FDCFIFO              ;
            JSR CHECKMSR_W
            BCS @ERR
            PLA                      ; Set last sector number
            STA FDCFIFO              ;
            JSR CHECKMSR_W
            BCS @ERR+1
            LDA #$1B                 ; Set intersector gap length
            STA FDCFIFO              ;
            JSR CHECKMSR_W
            BCS @ERR+1
            LDA #$FF                 ; Set datalength
            STA FDCFIFO              ;
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
            PLY
            PLX
            SEC
            RTS
@GO:        LDA #FDC::READID
            JSR FDCCOMMAND
            BCS @ERR
            JSR CHECKMSR_W
            BCS @ERR
            LDA FDCDRV
            ORA #$04
            STA FDCFIFO
            LDX #$00
@WAIT:      delay $05
            LDA FDCMSR
            AND #$D0
            CMP #$D0
            BNE @WAIT
@LOOP:      delay $ff
            LDA FDCFIFO
            STA FDCSR0,X
            INX
@READ:      LDA FDCMSR
            AND #$10
            BNE @LOOP
            LDA FDCSR0
            AND #$F0
            BNE @TRY
            BRA @DONE
@DONE:      CLC
@ERR:       LDA FDCSR0
            BRK
            .byte $00
            LDA FDCSR1
            BRK
            .byte $00
            LDA FDCSR2
            BRK
            .byte $00
;@DONE:
            PLY
            PLX
            RTS


;;FDCREADID:  PHX
;            PHY
;            LDY ;$04
;@TRY:       DEY
;            BNE @GO
;            LDA ;ERR::EIO
;            SEC
;            BRA @ERROR
;@GO:        LDA ;FDC::READID
;            JSR FDCCOMMAND
;            BCS @ERROR
;            JSR CHECKMSR_W
;            BCS @ERROR
;@READY:     LDA FDCDRV
;            STA FDCFIFO
;            JSR CHECKMSR_R
;            BCS @ERROR
;            LDX ;$00
;@LOOP:      LDA FDCFIFO
;            STA FDCSR0,X
;            INX
;            LDA FDCMSR
;            AND #$F0
;            CMP #$D0
;            BEQ @LOOP
;            LDA FDCSR0
;            AND ;$F0
;            BEQ @DONE
;            BRA @TRY
;@DONE:      CLC
;            PLY
;            PLX
;            RTS
;@ERROR:     BRK
;            .BYTE $00
;            PLY
;            PLX
;            RTS

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
FDCSNS:     LDA #FDC::SENSE         ; Send SENSE command
            JSR FDCCOMMAND
            BCS @END
            delay $40
            LDA FDCMSR              ; Sense interrupt will fail unless SEEK/RECAL is complete
            AND #$F0                ; In this case MSR will be 0x80
            CMP #$80                ;
            BEQ FDCSNS              ; If it is, issue the sense again
@DATA:      JSR CHECKMSR_R          ; Wait for data
            BCS @END
            LDA FDCFIFO             ;
            STA FDCSR0              ; Save SR0
            delay $40
            LDA FDCMSR              ; Sense interrupt will fail unless SEEK/RECAL is complete
            AND #$F0                ; In this case MSR will be 0x80
            CMP #$80                ;
            BEQ FDCSNS              ; If it is, issue the sense again
@RESULT:    JSR CHECKMSR_R          ; Get SR1
            BCS @END
            LDA FDCFIFO             ; Save SR1
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