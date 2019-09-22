.pc02

; MattMon
;debug=1
duart=1
video=1
;acia=1
via=1

.ifdef acia
ACIABASE =  $D110
ACIADR  =   $D110
ACIASR  =   ACIABASE+$1
ACIACMR =   ACIABASE+$2
ACIACNR =   ACIABASE+$3
.endif

.ifdef via
VIABASE =   $D120
VIAORA  =   VIABASE+$1
VIAORB  =   VIABASE
VIADDRB =   VIABASE+$2
VIADDRA =   VIABASE+$3
VIAT1CL =   VIABASE+$4
VIAT1CH =   VIABASE+$5
VIAT1LL =   VIABASE+$6
VIAT1LH =   VIABASE+$7
VIAT2CL =   VIABASE+$8
VIAT2CH =   VIABASE+$9
VIASR   =   VIABASE+$A
VIAACR  =   VIABASE+$B
VIAPCR  =   VIABASE+$C
VIAIFR  =   VIABASE+$D
VIAIER  =   VIABASE+$E
VIAORA2 =   VIABASE+$F
.endif

; Zero Page vars

FATSECTORBUFPTR = $E6
FATSECTORDEST   = $E8
SECTORBUFPTR    = $EA
FAT_FILENAMEPTR = $EC
FDPTR           = $EE ; Pointer into file descriptors
FD_FILENAMEPTR  = $F0
CREG            = $F2 ; Config Flags, 0x01 - Video Enable, 0x02 - Video Mode (0 - Mode 1 (640x480), 1 - Mode 2 (640x400)), 0x04 - 8x8 Font selection (Mode 2 only)
CHARPTRL        = $F3 ; Pointer to character in VRAM, usually VIDPTR+CURSX
CHARPTRH        = $F4
VIDPTRL         = $F5 ; Pointer to current line in VRAM
VIDPTRH         = $F6
STARTL          = $F7
STARTH          = $F8
ENDL            = $F9
ENDH            = $FA
STRLO           = $FD
STRHI           = $FE

INBUF           = $0200 ; 0200-027F 128 Byte Buffer
CMDBUF          = $0280
INPBUFHEAD      = $0300 ; key buffer index
INPBUFTAIL      = $0501
CMDBUFHEAD      = $0502
CMDBUFPTR       = $0503
HEXTMP          = $0504
CHECKSUM        = $0505
CURSX           = $0506
IRQ_softVector  = $0507 ; Soft vector allows wedging in an IRQ Handler
IRQ_softVectorH = $0508
SCROLLLO        = $0509
SCROLLHI        = $050A
COLOR           = $050B
JIFFIES         = $050C
SECONDSL        = $050D
SECONDSH        = $050E
IOTMR           = $050F



FDS              = $0900
NEXTSECTOR       = $0980
LBACYL           = NEXTSECTOR+2
LBAHEAD          = LBACYL+1
LBASECT          = LBAHEAD+1
FDCTRIES         = LBASECT+1
;;; FDC Vars
FDCSR0           = FDCTRIES+1
FDCSR1           = FDCSR0+1
FDCSR2           = FDCSR1+1
FDCTRN           = FDCSR2+1
FDCHDN           = FDCTRN+1
FDCSCN           = FDCHDN+1
FDCBPS           = FDCSCN+1
FDCEOT           = FDCBPS+1
FDCGPL           = FDCEOT+1
FDCDTL           = FDCGPL+1
FDCDRV           = FDCDTL+1


SECTORBUF       = $0A00
FATBUF          = $0C00


.ifdef duart
DUART        = $D000
DUARTMRA     = DUART
DUARTSRA     = DUART+$1
DUARTCSRA    = DUART+$1
DUARTCRA     = DUART+$2
DUARTRXA     = DUART+$3
DUARTTXA     = DUART+$3
DUARTIPCR    = DUART+$4
DUARTACR     = DUART+$4
DUARTISR     = DUART+$5
DUARTIMR     = DUART+$5
DUARTCTU     = DUART+$6
DUARTCTPU    = DUART+$6
DUARTCTL     = DUART+$7
DUARTCTPL    = DUART+$7
DUARTMRB     = DUART+$8
DUARTSRB     = DUART+$9
DUARTCSRB    = DUART+$9
DUARTCRB     = DUART+$A
DUARTRXB     = DUART+$B
DUARTTXB     = DUART+$B
DUARTIVEC    = DUART+$C
DUARTIPR     = DUART+$D
DUARTSOPR    = DUART+$E
DUARTROPR    = DUART+$F
.endif

.SEGMENT "ROMCODE"

; Pointers to functions so that ROM changes don't break programs
V_CHARIN:   JMP CHARIN      ; Char in
V_CHAROUT:  JMP CHAROUT     ; Char our
V_CRLF:     JMP CRLF        ; Print CRLF
V_LOAD:     JMP LOAD        ; LOAD (do nothing)
V_SAVE:     JMP SAVE        ; SAVE (do nothing)
V_CLS:      JMP CLS         ; Clear screen
V_HEXTOASC: JMP HEXTOASC    ; HEX byte to Ascii
V_CHARINW:  JMP CHARINW     ; Char in (wait)
V_HEXOUT:   JMP HEXOUT      ; Hex out
V_CHARINU:  JMP CHARINU     ; Char in uppercase
V_FOPEN:    JMP FOPEN
V_FCLOSE:   JMP FDFREE
V_FREAD:    JMP FREAD
V_ERRMSG:   JMP PRERR

.include "errors.inc"
.include "math.inc"
.include "macros.inc"
.include "disk.inc"
.include "fat.inc"
.include "FD.inc"
.include "video.inc"

PRERR:      JMP ERRMSG::PRINT

COLDSTRT:   SEI             ;   Disable interrupts
            LDA #<IRQ_ROM
            STA IRQ_softVector
            LDA #>IRQ_ROM
            STA IRQ_softVector+1
            LDX #$FF        ;
            TXS             ;   Init Stack Pointer
            LDX #$00
            STZ SECONDSL
            STZ SECONDSH
            STZ JIFFIES
            STZ INPBUFHEAD
            STZ INPBUFTAIL
            STZ CMDBUFHEAD
            STZ CMDBUFPTR

STACKINIT:  STZ $0100,X
            INX
            BNE STACKINIT
.ifdef duart
DUARTINIT:  LDA #$B0          ; Reset MRA Pointer to $00
            STA DUARTCRA
            LDA #$C9          ; Enable RX Watchdog, RX Interrupt level 16 bytes, 16 byte FIFO, Baud Extended mode I
            STA DUARTMRA
            LDA #$D3          ; Enable RX controlled RTS, RX Interrupt level 16 bytes, No parity, 8 Bits per character
            STA DUARTMRA
            LDA #$01
            STA DUARTSOPR
            LDA #$07          ; Enable TX CTS Control, 1 Stop Bit
            STA DUARTMRA
            LDA #$60          ; Set timer mode external clock x1
            STA DUARTACR
            LDA #$CC          ; Set TX/RX Baud to 230,400 (115200 with 1.8432Mhz Oscillator)
            STA DUARTCSRA
            LDA #$0A          ; Enable Timer/RxRDYA Interrupts
            STA DUARTIMR
            LDA #$24          ; 100hz interval for Timer
            STA DUARTCTPU
            STZ DUARTCTL
            LDA DUARTSOPR     ; Start timer
            LDA #$05
            STA DUARTCRA      ; Enable TX/RX on Channel A
            LDA #$0A
            STA DUARTCRB      ; Disable TX/RX on Channel B
.endif
.ifdef acia
ACIAINIT:   LDA #$10        ;
            STA ACIACNR     ;   19200 Baud, 8 Bits, 1 Stop bit
            LDA #$09        ;
            STA ACIACMR     ;   DTR low, RTSB low, tx interrupt disabled
.endif
.ifdef via
VIAINIT:    LDA #$7F        ;
            STA VIAIFR      ;   Clear and disable all VIA Interrupts
            STA VIAIER      ;
            LDX #$0D        ;   Zero out VIA Registers 0x00-0x0C
@LOOP:      DEX
            STZ VIABASE,X
            BNE @LOOP
            LDA #$82        ;   Enable PortA Interrupt on CA1 high with input latching for keyboard
            STA VIAIER
            LDA #$FF
            STA VIAIFR
            LDA #$01
            STA VIAACR
            STZ VIAPCR
            LDA #$FF
            STA VIADDRB
.endif
.ifdef video
            JSR VIDINIT
.endif

            CLI
INITFLOPPY: JSR FDZERO
            JSR FDCINIT
            BCC MAIN
            printmsg "FDC ERROR"
MAIN:       LDA #$00
            JSR STROUT
            JSR CRLF
            JMP PROMPT

CHARIN:     PHX
            JSR CHARREADY
            BEQ @NOCHAR
            LDA INBUF,X
            INX
            BPL @NOWRAP
            LDX #$00
@NOWRAP:    STX INPBUFTAIL
.ifdef duart
            JSR CTSCONTROL
.endif
            PLX
            CMP #$00
            SEC
            RTS
@NOCHAR:    PLX
            CLC
            RTS

CHARINW:    JSR CHARIN
            BCC CHARINW
            RTS

CHARINU:    JSR CHARIN
            BCC @END+1
            CMP #'a'
            BCC @END
            CMP #'z'+1
            BCS @END
            EOR #$20
@END:       SEC
            RTS

CHAROUT:
.ifdef video
            PHA
            JSR COUTVIDEO
            PLA
.endif
CHAROUTS:   CMP #$7F
            BNE @NOTDEL
            LDA #$08
@NOTDEL:
.ifdef acia
ACIATX:     PHA
            LDA ACIASR
            AND #$10
            BEQ ACIATX+1
            PLA
            STA ACIADR
.ifdef wdcfix
            JSR DELAY_6551
.endif
.endif
.ifdef duart
DUARTTX:    PHA
            LDA DUARTSRA
            AND #$04
            BEQ DUARTTX+1
            PLA
            STA DUARTTXA
.endif
            RTS

STROUT:     PHX
            PHY
            ASL A
            TAX
            LDA STR_TABLE,X
            STA STRLO
            LDA STR_TABLE+1,X
            STA STRHI
            LDY #$00
STRLOOP:    LDA (STRLO),Y
            BEQ STREND
            CMP #$FF
            BNE @TEXT
            INY
            LDA (STRLO),Y
            STA COLOR
            INY
            BRA STRLOOP
@TEXT:      JSR CHAROUT
            INY
            BRA STRLOOP
STREND:     PLY
            PLX
            RTS

; Print string following the JSR to this
PRIMM:
      pla               ; get low part of (string address-1)
      sta   STRLO
      pla               ; get high part of (string address-1)
      sta   STRHI
      bra   primm3
primm2:
      jsr   CHAROUT        ; output a string char
primm3:
      inc   STRLO         ; advance the string pointer
      bne   primm4
      inc   STRHI
primm4:
      lda   (STRLO)       ; get string char
      bne   primm2      ; output and continue if not NUL
      lda   STRHI
      pha
      lda   STRLO
      pha
      rts               ; proceed at code following the NUL


CRLF:       LDA #$0A
            JSR CHAROUT
            LDA #$0D
            JSR CHAROUT
            RTS

HEXTOASC:   PHA
            LSR
            LSR
            LSR
            LSR
            AND #$0F
            JSR HEXOUT
            PLA
            PHA
            AND #$0F
            JSR HEXOUT
            PLA
            RTS

HEXOUT:     PHA
            CLC
            CMP #$0A
            BCC HEXL        ;   If 0-9, Add 0x30 to get Ascii code
            ADC #$06        ;   If A-F, Add 0x36 (plus carry!) to get the proper ascii code
HEXL:       ADC #$30
            JSR CHAROUT
            PLA
            RTS

BINGET:     PHX
            PHY
            LDY #$08
            STZ HEXTMP
            LDA #$00
            PHA
            LDX CMDBUFPTR
@LOOP:      LDA CMDBUF,X
            SEC
            SBC #'0'
            CMP #$02
            BCS @ERROR
            INX
            LSR A
            PLA
            ROL A
            PHA
            DEY
            BNE @LOOP
            STX CMDBUFPTR
            SEC
@END:       PLA
            PLY
            PLX
            RTS
@ERROR:     CLC
            BRA @END

BINPUT:     PHA
            LDY #$08
            PHA
@LOOP:      PLA
            ROL
            PHA
            LDA #'0'
            BCC @ZERO
            INC A
@ZERO:      JSR CHAROUT
            DEY
            BNE @LOOP
            PLA
            PLA
            RTS

HEXGET:     PHX
            LDX CMDBUFPTR
            LDA CMDBUF,X
            JSR NIBBLE
            BCC @END
            ASL
            ASL
            ASL
            ASL
            STA HEXTMP
            INX
            LDA CMDBUF,X
            JSR NIBBLE
            BCC @END
            ORA HEXTMP
            SEC
@END:       INX
            STX CMDBUFPTR
            PLX
            RTS

ISHEX:
NIBBLE:     SEC
            SBC #'0'
            CMP #$0A
            BCC @NUM
            SEC
            SBC #'A'-':'
            CMP #$0F+1
            BCS @ERROR
            CMP #$0A
            BCC @ERROR
@NUM:       SEC
            RTS
@ERROR:     CLC
            RTS

@END:       JMP PROMPT

HEXIN:      PHX
            LDX #$02
@HEXNIBBLE: JSR CHARINU
            BCC @HEXNIBBLE
            CMP #$1B    ; ESC
            BEQ @HEXERR
            CMP #$3A        ; CHAR(9)+1
            BCS @HEXINAF
            CMP #$30
            BCC @HEXNIBBLE
            AND #$0F
            BRA @HEXEND
@HEXINAF:   SBC #$37
            CMP #$0A
            BCC @HEXNIBBLE
            CMP #$10
            BCS @HEXNIBBLE
@HEXEND:    JSR HEXOUT
            DEX
            BEQ @HEXINLO
            ASL
            ASL
            ASL
            ASL
            STA HEXTMP
            BRA @HEXNIBBLE
@HEXINLO:   ORA HEXTMP
            CLC
            PLX
            RTS
@HEXERR:    SEC
            PLX
            RTS

UNKNOWN:    JSR CRLF
            JSR CRLF
            LDA #$09
            JSR STROUT

PROMPT:     JSR CRLF
            LDA #']'
            JSR CHAROUT
            LDX #$00
            STZ CMDBUFPTR
            STZ CMDBUF,X
@PROMPTL:   JSR CHARINU
            BCC @PROMPTL
            CMP #$0D
            BEQ @GO
            CMP #$7F
            BEQ @BS
            CMP #$08
            BNE @STORE
@BS:        DEX
            BPL @NOERR
            LDA #$07
            JSR CHAROUT
            INX
            BRA @PROMPTL
@NOERR:     JSR CHAROUT
            PHA
            LDA #' '
            JSR CHAROUT
            PLA
            JSR CHAROUT
            STZ CMDBUF,X
            BRA @PROMPTL
@STORE:     CMP #' '
            BCC @PROMPTL
            CMP #$7F
            BCS @PROMPTL
            JSR CHAROUT
            STA CMDBUF,X
            INX
            BPL @PROMPTL
            DEX
            STZ CMDBUF,X
            LDA #$07
            JSR CHAROUT
            LDA #$7F
            JSR CHAROUT
            BRA @PROMPTL
@GO:        STZ CMDBUF,X
            LDX #$FF
@SEARCH:    INX
            LDA T_COMMAND_TAB,X
            BEQ UNKNOWN
            CMP CMDBUF
            BNE @SEARCH
            LDA CMDBUF+1
            PHA
            TXA
            ASL A
            TAX
            LDA #$02
            STA CMDBUFPTR
            JSR CRLF
            PLA
            JMP (T_COMMAND_ADDR,X)
@ERROR:     LDA #ERR::ESYNTAX
            JSR ERRMSG::PRINT
            JMP PROMPT

BARS:       
.ifdef video
            BNE @SYNTAXERR
            JSR CRLF
            LDA #$00
            JSR PRINTBARS
            LDA #$08
            JSR PRINTBARS
            LDA #PROMPTCOLOR
            STA COLOR
.endif
            JMP PROMPT
@SYNTAXERR: JMP SYNTAXERR

PRINTBARS:  STA STARTL
            LDY #$0E
;
@LINES:     ;JSR CRLF
            LDA STARTL
            PHY
@BARS:      LDY #$08
            LDA STARTL
;
@NEXTBAR:   ROL
            ROL
            ROL
            ROL
            AND #$F0
            STA COLOR
            LDX #$0A
@BARLOOP:   PHA
            LDA #' '
            JSR CHAROUT
            PLA
            DEX
            BNE @BARLOOP
            ROR
            ROR
            ROR
            ROR
            INC A
            DEY
            BNE @NEXTBAR
;
            PLY
            DEY
            BNE @LINES
;
            RTS

SLEEP:      CMP #' '
            BEQ @SYNTAXOK
            JMP SYNTAXERR
@SYNTAXOK:  JSR HEXGET
            BCC @EXIT
            PHA
            LDA SECONDSH
            STA STARTH
            PLA
            CLC
            ADC SECONDSL
            STA STARTL
            BCC @WAIT
            INC SECONDSH
@WAIT:      LDA SECONDSH
            CMP STARTH
            BNE @WAIT
            LDA SECONDSL
            CMP STARTL
            BNE @WAIT
@EXIT:      JMP PROMPT


FILL:       CMP #' '
            BEQ @SYNTAXOK
            JMP SYNTAXERR
@SYNTAXOK:  JSR HEXGET
            STA STARTH
            JSR HEXGET
            STA STARTL
            INC CMDBUFPTR
            JSR HEXGET
            STA ENDH
            JSR HEXGET
            STA ENDL
            INC ENDL
            BNE @NOENDWRAP
            INC ENDH
@NOENDWRAP: INC CMDBUFPTR
            JSR HEXGET
            PHA
            LDY #$00
@LOOP:      PLA
            PHA
            STA (STARTL),Y
            INC STARTL
            BNE @NOWRAP
            INC STARTH
@NOWRAP:    LDA STARTH
            CMP ENDH
            BNE @LOOP
            LDA STARTL
            CMP ENDL
            BNE @LOOP
            PLA
            JMP PROMPT

DISPLEND2:  JMP SYNTAXERR

READ16:     LDA STARTL
            SEC
            ADC #$FF
            STA ENDL
            LDA STARTH
            ADC #$00
            STA ENDH
            BRA GO_DISPLAY
DISPLAYRAM: CMP #' '
            BNE DISPLEND2
            JSR HEXGET
            BCC DISPLEND2
            STA STARTH
            JSR HEXGET
            BCC DISPLEND2
            AND #$F0
            STA STARTL
            LDX CMDBUFPTR
            LDA CMDBUF,X
            CMP #' '
            BNE READ16
            INX
            STX CMDBUFPTR
            JSR HEXGET
            BCC DISPLEND2
            STA ENDH
            JSR HEXGET
            BCC DISPLEND2
            ORA #$0F
            INC
            BNE @NOCARRY
            INC ENDH          
@NOCARRY:   STA ENDL
GO_DISPLAY: JSR CRLF
            LDA #$02
            JSR STROUT
            LDA #$09          ; TAB
            JSR CHAROUT
            LDX #$10
            LDY #$00
DISPLOOP1:  TYA               ; Loop1: Display Lower Address Nibble
            JSR HEXTOASC
            LDA #' '
            JSR CHAROUT
            INY
            DEX
            BNE DISPLOOP1
            JSR CRLF
            LDA #'-'          ; Loop2: Draw horizontal rule
            LDX #$37          ;
DISPLOOP2:  JSR CHAROUT       ;
            DEX               ;
            BNE DISPLOOP2     ;
DISPLINE:   LDX #$10          ; Display Bytes at START,Y
            LDY #$00
            JSR CRLF
            LDA #'$'          ; Display Address, i.e "$0010"
            JSR CHAROUT       ;
            LDA STARTH  ;
            JSR HEXTOASC      ;
            LDA STARTL  ;
            JSR HEXTOASC      ;
            LDA #$09          ; TAB
            JSR CHAROUT       ;
DISPLINE1:  LDA (STARTL),Y    ; Display 16 Bytes 0-F for each line
            JSR HEXTOASC            ;
            LDA #' '                ;
            JSR CHAROUT
            INC STARTL
            BNE @NOWRAP
            INC STARTH
@NOWRAP:    LDA STARTH
            CMP ENDH
            BNE @NOMATCH
            LDA STARTL
            CMP ENDL
            BCS DISPLEND
@NOMATCH:   DEX                     ;
            BNE DISPLINE1           ; If we have displayed <16 Bytes keep looping
                      
            BRA DISPLINE            ;
DISPLEND:   JMP PROMPT

READMEM:    CMP #' '
            BEQ @SYNTAXOK
            JMP SYNTAXERR
@SYNTAXOK:  JSR CRLF
            JSR HEXGET
            BCC @ERROR
            STA STARTH
            JSR HEXGET
            BCC @ERROR
            STA STARTL
            LDX CMDBUFPTR
            LDA CMDBUF,X
            BEQ @GO
            CMP #' '
            BNE @ERROR
            INX
            STX CMDBUFPTR
            LDA CMDBUF,X
            CMP #'%'
            BNE @ERROR
@GO:        LDA #'$'
            JSR CHAROUT
            LDA STARTH
            JSR HEXTOASC
            LDA STARTL
            JSR HEXTOASC
            LDA #':'
            JSR CHAROUT
            LDA (STARTL)
            PHA
            LDA CMDBUF,X
            CMP #'%'
            BNE @HEX
            LDA #'%'
            JSR CHAROUT
            PLA
            JSR BINPUT
            BRA @EXIT
@HEX:       PLA
            JSR HEXTOASC
@EXIT:      JMP PROMPT
@ERROR:     LDA #ERR::ESYNTAX
            JSR ERRMSG::PRINT
            BRA @EXIT

WRITEMEM:   CMP #' '
            BNE @ERROR
            JSR HEXGET
            BCC @ERROR
            STA STARTH
            JSR HEXGET
            BCC @ERROR
            STA STARTL
            LDX CMDBUFPTR
            LDA CMDBUF,X
            CMP #' '
            BNE @ERROR
            INX
            STX CMDBUFPTR
            LDA CMDBUF,X
            CMP #'%'
            BNE @HEX
            INX
            STX CMDBUFPTR
            JSR BINGET
            BCC @ERROR
            PHA
            LDX CMDBUFPTR
            LDA CMDBUF,X
            BNE @ERROR
            PLA
            STA (STARTL)
            BRA @EXIT
@HEX:       JSR HEXGET
            BCC @ERROR
            STA (STARTL)
@WRLOOP:    LDX CMDBUFPTR
            LDA CMDBUF,X
            BEQ @EXIT
            CMP #' '
            BNE @ERROR
            INX
            STX CMDBUFPTR
            INC STARTL
            BNE @NOWRAP
            INC STARTH
@NOWRAP:    JSR HEXGET
            BCC @ERROR
            STA (STARTL)
            BRA @WRLOOP
@EXIT:      JMP PROMPT
@ERROR:     JMP SYNTAXERR

JUMP:       CMP #' '
            BNE @SYNTAX
            LDA #$04
            JSR STROUT
            JSR HEXGET
            BCC @EXIT
            ;JSR HEXTOASC
            STA STARTH
            JSR HEXGET
            BCC @EXIT
            ;JSR HEXTOASC
            STA STARTL
            JSR CRLF
            JSR CRLF
            JMP (STARTL)
@EXIT:      JMP PROMPT
@SYNTAX:    LDA #ERR::ESYNTAX
            JSR ERRMSG::PRINT
            BRA @EXIT

MEMTEST:    BEQ @SYNTAXOK
            JMP SYNTAXERR
@SYNTAXOK:  LDA #$01
            PHA
            LDX #$08
@MEMLOOP:   LDA #$10
            STA STARTH
            STZ STARTL
            LDY #$00
            PLA
            PHA
@WLOOP2:    STA (STARTL), Y
            INY
            BNE @WLOOP2
            INC STARTH
            LDA STARTH
            CMP #$A0
            BEQ @READ
            PLA
            PHA
            BRA @WLOOP2
@READ:      LDY #$00
            LDA #$10
            STA STARTH
            STZ STARTL
@RLOOP:     PLA
            PHA
            CMP (STARTL), Y
            BNE @ERROR
@RLOOPRET:  INY
            BNE @RLOOP
            INC STARTH
            LDA STARTH
            CMP #$A0
            BEQ @END
            BRA @RLOOP
@ERROR:     LDA #'!'
            JSR CHAROUT
            LDA STARTH
            JSR HEXTOASC
            TYA
            JSR HEXTOASC
            LDA #' '
            JSR CHAROUT
            PLA
            PHA
            JSR HEXTOASC
            LDA #' '
            JSR CHAROUT
            LDA (STARTL),Y
            JSR HEXTOASC
            JSR CRLF
            BRA @RLOOPRET
@END:       PLA
            ASL
            PHA
            DEX
            BNE @MEMLOOP
            PLA
            JMP PROMPT

C_CLS:      BEQ @SYNTAXOK
            JMP SYNTAXERR
@SYNTAXOK:  JSR CLS
            JMP PROMPT

CLS:        PHY
            LDA #$A0
            STA STARTH
            STZ STARTL
            LDY #$00
            LDA #$20
@LOOP:      LDA #$20
            STA (STARTL), Y
            INY
            LDA COLOR
            STA (STARTL), Y
            INY
            BNE @LOOP
            INC STARTH
            LDA STARTH
            CMP #$C0
            BEQ @END
            LDA #$20
            BRA @LOOP
@END:       PLY
            RTS

SETMODE:    BNE SYNTAXERR
            JSR VIDMODE
            JMP PROMPT

SYNTAXERR:  LDA #ERR::ESYNTAX
            JSR ERRMSG::PRINT
            JMP PROMPT

UNLOADER:   LDA #$01
            STA $FAFA
            JMP ($FFFC)
UNLOADED:

UNLOAD:     BEQ @SYNTAXOK
            JMP SYNTAXERR
@SYNTAXOK:  LDY #UNLOADED-UNLOADER
@LOOP:      LDA UNLOADER,Y
            STA $1000,Y
            DEY
            BPL @LOOP
            JMP $1000


LOADFILE:   CMP #' '
            BNE @STXERR
            LDA #<CMDBUF
            CLC
            ADC #$02
            STA FD_FILENAMEPTR
            LDA #>CMDBUF
            ADC #$00
            STA FD_FILENAMEPTR+1
            JSR FOPEN
            BCS @ERROR
            LDY #$00
            JSR FREAD
            BCS @ERROR
            CMP #$CA
            BNE @NOTEXEC
            JSR FREAD
            BCS @ERROR
            CMP #$C2
            BNE @NOTEXEC
            JSR FREAD
            BCS @ERROR
            STA STARTL
            STA ENDL
            JSR FREAD
            BCS @ERROR
            STA STARTH
            STA ENDH
@RLOOP:     JSR FREAD
            BCS @ERROR
            LDY #$00
            STA (STARTL),Y
            CLC
            LDA STARTL
            ADC #$01
            STA STARTL
            LDA STARTH
            ADC #$00
            STA STARTH
            BRA @RLOOP
@STXERR:    LDA #ERR::ESYNTAX
@ERROR:     CMP #$00
            BEQ @END
            PHA
            JSR FDFREE
            PLA
            SEC
            JSR ERRMSG::PRINT
            JMP PROMPT
@END:       JSR FDFREE
            CLC
            JMP (ENDL)
@NOTEXEC:   LDA #ERR::ENOEXEC
            BRA @ERROR

;## Motorola S-Record Loader
SREC:       BEQ @SYNTAXOK
            LDA #ERR::ESYNTAX
            JSR ERRMSG::PRINT
            JMP PROMPT
@SYNTAXOK:  JSR CRLF
            LDA #$05
            JSR STROUT
@START:     JSR CHARINU
            BCC @START
            CMP #$1B
            BNE @GO
            JMP @END
@GO:        CMP #'S'
            BNE @START
@START1:    JSR CHARINU
            BCC @START1
            CMP #$1B
            BEQ @END
            CMP #'0'
            BNE @S9
            JMP @MESSAGE
@S9:        CMP #'9'
            BNE @S1
            JMP @EXECUTE
@S1:        CMP #'1'
            BNE @IGNORE
            LDA #'#'
            JSR CHAROUT
            JSR HEXINS ; Byte Count
            BCS @END
            STA CHECKSUM
            TAX
            DEX
            JSR HEXINS ; Address High Byte
            BCS @END
            STA STARTH
            DEX
            CLC
            ADC CHECKSUM
            CLC
            STA CHECKSUM
            JSR HEXINS ; Address Low Byte
            STA STARTL
            DEX
            CLC
            ADC CHECKSUM
            CLC
            STA CHECKSUM
            LDY #$00
@DLOOP:     JSR HEXINS
            STA (STARTL),Y
            CLC
            ADC CHECKSUM
            CLC ; Don't care about the carry
            STA CHECKSUM
            INY
            DEX
            BNE @DLOOP
            LDA #$FF
            EOR CHECKSUM ; One's complement Checksum
            STA CHECKSUM
            JSR HEXINS ; Senders Checksum
            CMP CHECKSUM        ; Compare Senders checksum with our own
            BEQ @CHECKSUMOK
            JSR CRLF
            LDA #$06
            JSR STROUT
            LDA STARTH
            JSR HEXTOASC
            LDA STARTL
            JSR HEXTOASC
            JSR CRLF
@CHECKSUMOK:JMP @START
@END:       LDA #$07
            JSR STROUT
            JMP PROMPT
@IGNORE:    JSR HEXINS  ; Pay no attention to non S0,S1,S9 records
            BCS @END
            TAX
@IGLOOP:    JSR HEXINS
            DEX
            BNE @IGLOOP
            JMP @START
@EXECUTE:   JSR HEXINS
            JSR CRLF
            LDA #$04
            JSR STROUT
            JSR HEXIN
            STA STARTH
            JSR HEXIN
            STA STARTL
            JSR HEXINS
            STZ INPBUFHEAD
            STZ INPBUFTAIL
            JMP (STARTL)
@MESSAGE:   JSR CRLF
            LDA #$08
            JSR STROUT
            JSR HEXINS  ; Byte count
            TAX
            DEX
            DEX
            DEX
            JSR HEXINS  ; Address
            JSR HEXINS  ; Address
@GETCHAR:   JSR HEXINS
            JSR CHAROUT
            DEX
            BNE @GETCHAR
            LDA #'.'
            JSR CHAROUT
            JSR CRLF
            JSR HEXINS ; Checksum
            JMP @START

HEXINS:     PHX
            LDX #$02
@HEXNIBBLE: JSR CHARINU
            BCC @HEXNIBBLE
            CMP #$1B    ; ESC
            BEQ @HEXERR
            CMP #$3A        ; CHAR(9)+1
            BCS @HEXINAF
            CMP #$30
            BCC @HEXNIBBLE
            AND #$0F
            BRA @HEXEND
@HEXINAF:   SBC #$37
            CMP #$0A
            BCC @HEXNIBBLE
            CMP #$10
            BCS @HEXNIBBLE
@HEXEND:    DEX
            BEQ @HEXINLO
            ASL
            ASL
            ASL
            ASL
            AND #$F0
            STA HEXTMP
            BRA @HEXNIBBLE
@HEXINLO:   ORA HEXTMP
            PLX
            CLC
            RTS
@HEXERR:    SEC
            PLX
            RTS

;## End of S-Record loader

CHARREADY:  LDX INPBUFTAIL
            CPX INPBUFHEAD
            RTS

INPBUFSZ:   PHA
            LDA INPBUFHEAD
            SEC
            SBC INPBUFTAIL
            AND #$7F
            TAX
            PLA
            RTS
.ifdef duart
CTSCONTROL: PHA
            JSR INPBUFSZ
            CPX #$7F
            BCC @ENABLE
            LDA #$08
            STA DUARTIMR ; Disable RX Full interrupt until INPBUF has more space
            LDA #$07
            JSR CHAROUT
            SEC
            BRA @END
@ENABLE:    LDA #$0A
            STA DUARTIMR ; Enable RX Full interrupt
@END:       PLA
            RTS
.endif

;#### Handle IRQs
BRKTAG:     LDA #' '
            JSR CHAROUT
            LDA #'#'
            JSR CHAROUT
            LDA $0105,X
            SEC
            SBC #$01
            STA STARTL
            LDA $0106,X
            SBC #$00
            STA STARTH
            LDA (STARTL)
            JSR HEXTOASC
            JSR CRLF
            RTS

BRKINT:     LDA $0104,X     ;   Read Processor status from Stack
            EOR #$10        ;   Clear BRK bit in processor Status
            STA $0104,X     ;   Push Status back to the Stack
            JSR CRLF
            LDA #$01
            JSR STROUT
            LDA $0105,X    ; Decrement saved PC value to get BRK location
            SEC            ; We first subtract 2 from the low byte
            SBC #$02       ; If Low byte wraps, decrement High byte
            PHA            ; Push low byte to stack
            LDA $0106,X
            SBC #$00
            JSR HEXTOASC   ; Print High Byte
            PLA
            JSR HEXTOASC   ; Print Low Byte
            LDA #' '
            JSR CHAROUT
            LDY #$04
            PHX
@LOOP:      LDA T_AXYP-1,Y
            JSR CHAROUT
            LDA #':'
            JSR CHAROUT
            LDA $0104,X
            DEX
            JSR HEXTOASC
            LDA #' '
            JSR CHAROUT
            DEY
            BNE @LOOP
            LDA #'S'
            JSR CHAROUT
            LDA #':'
            JSR CHAROUT
            PLX
            TXA
            JSR HEXTOASC
            JSR BRKTAG
            LDA #$0A
            JSR STROUT
            CLI
@GETCHAR:   JSR CHARINW
            CMP #'R'
            BNE @NOTR
            JMP ($FFFC)
@NOTR:      CMP #'C'
            BEQ ENDIRQ
            CMP #'M'
            BNE @GETCHAR
            JMP PROMPT
@END:       JMP ENDIRQ

IRQ:        PHA
            PHX
            PHY
            TSX
            LDA $0104,X     ;   Read Status register, check if this IRQ was caused by a BRK
            AND #$10
            BNE BRKINT
            JMP (IRQ_softVector) ; Defaults to IRQ_ROM
IRQ_ROM:
.ifdef duart
            LDA DUARTISR
            AND #$0A
            BNE DUARTIRQ
.endif
.ifdef acia
            LDA ACIASR      ;   Check if the ACIA generated the Interrupt
            BMI ACIARCV
.endif
.ifdef via
@VIA:       LDA VIAIFR      ;   Check if the VIA generated the Interrupt
            BMI VIARCV
.endif
ENDIRQ:     PLY
            PLX
            PLA
            CLI
            RTI

.ifdef duart
DUARTIRQ:   LDA DUARTISR
            AND #$08        ; Timer interrupt?
            BEQ DUARTRCV    ; Nope, receive a character

TICK:       LDA JIFFIES
            CMP #99
            BNE @NOWRAP
            STZ JIFFIES
            LDA IOTMR
            CMP #$FF
            BEQ @IOTMREND
            CLC
            ADC #$01
            STA IOTMR
            CMP #$02
            BNE @IOTMREND
            JSR FDCDSELECT
@IOTMREND:
            ;INC IOTMR
            INC SECONDSL
            BNE @TICKEND
            INC SECONDSH
            BRA @TICKEND
@NOWRAP:    INC JIFFIES
@TICKEND:   LDA DUARTROPR ; "reset counter" command

DUARTRCV:   LDA DUARTSRA
            AND #$01
            BEQ DUARTEND
            JSR CTSCONTROL
            BCS DUARTEND
            LDA DUARTRXA
            JSR STORCHAR
            BRA DUARTRCV ; Loop around and get everything from the FIFO
DUARTEND:   JMP ENDIRQ
.endif

.ifdef acia
ACIARCV:    AND #$07  ; check for error and exit, I should really get around to handling errors...
            BNE @END
            LDA ACIADR
            JSR STORCHAR
@END:       JMP ENDIRQ
.endif

STORCHAR:   JSR INPBUFSZ
            CPX #$7F
            BCS @END
            LDX INPBUFHEAD
            STA INBUF,X
            INX
            BPL @NOWRAP
            LDX #$00
@NOWRAP:    STX INPBUFHEAD
@END:
.ifdef duart
            JSR CTSCONTROL
.endif
            RTS

.ifdef via        
VIARCV:     AND #$02       ;   Check if the Interrupt is due to an error
            BEQ VIAERR
            BIT #$40
            BNE VIAT1       ;   Should eventually handle this properly...
            LDA VIAORA      ;   Not an error, read ASCII character from IRA
            JSR STORCHAR
            LDA #$02
            STA VIAIFR
            JMP ENDIRQ

VIAERR:     JMP ENDIRQ

VIAT1:      LDA VIAT1CL
            JMP ENDIRQ
.endif

;#### END OF IRQ


LOAD:       JSR FOPEN
            BCC @OK
@ERR:       JSR ERRMSG::PRINT
            RTS
@OK:        JSR FREAD
            BCS @ERR
            BEQ @DONE
            JSR CHAROUT
            BRA @OK
@DONE:      RTS
LOADF:
SAVE:
SAVEF:     RTS


.ifdef wdcfix
DELAY_6551:     PHY                 ;   Save Y Reg (3 clk cycles)
                PHX                 ;   Save X Reg (3 clk cycles)
DELAY_LOOP:     LDY #05             ;   Get delay value (clock rate in MHz 2 clock cycles)

MICROSECOND:    LDX #$68            ;   Seed X reg for 1ms (2 clk cycles)
DELAY_1:        DEX                 ;   Decrement low index (2 clk cycles)
                BNE DELAY_1         ;   Loop back until done (3 clk cycles if yes, 2 clock)
                DEY                 ;   Decrease by one (2 clk cycles)
                BNE MICROSECOND     ;   Loop until done (3 clk cycles if yes, 2 clock)
                PLX                 ;   Restore X Reg (4 clk cycles)
                PLY                 ;   Restore Y Reg (4 clk cycles)
DELAY_DONE:     RTS
.endif

HELP:    JSR CRLF
         LDX #$00
         LDY #$00
@HLOOP:  LDA T_COMMAND_TAB,X
         BEQ @END
         JSR CHAROUT
         LDA #' '
         JSR CHAROUT
         LDA #'-'
         JSR CHAROUT
         LDA #' '
         JSR CHAROUT
@PRLOOP: LDA T_COMMAND_HELP_TEXT,Y
         BEQ @NEXT
         JSR CHAROUT
         INY
         BRA @PRLOOP
@NEXT:   INY
         JSR CRLF
         JSR CHAROUT
         INX
         BRA @HLOOP
@END:    JMP PROMPT

TESTWR:  LDA #$F9
         STA SECTORBUFPTR+1
         STZ SECTORBUFPTR
         LDA #$0B
         STA NEXTSECTOR+1
         LDA #$3F
         STA NEXTSECTOR
         JSR FDCWRITELBA
         LDA #$50
         STA SECTORBUFPTR+1
         STZ SECTORBUFPTR
         LDA #$0B
         STA NEXTSECTOR+1
         LDA #$3F
         STA NEXTSECTOR
         JSR FDCREADLBA
         JMP PROMPT

.SEGMENT "ROMDATA"
T_COMMAND_TAB:
.byte 'B'
.byte 'C'
.byte 'D'
.byte 'E'
.byte 'F'
.byte 'J'
.byte 'L'
.byte 'M'
.byte 'R'
.byte 'S'
.byte 'U'
.byte 'V'
.byte 'W'
.byte '?'
.byte $00

T_COMMAND_ADDR:
 .WORD BARS
 .WORD C_CLS
 .WORD DISPLAYRAM
 .WORD LOADFILE
 .WORD FILL
 .WORD JUMP
 .WORD SREC
 .WORD MEMTEST
 .WORD READMEM
 .WORD SLEEP
 .WORD UNLOAD
 .WORD SETMODE
 .WORD WRITEMEM
 .WORD HELP
 .WORD PROMPT

T_COMMAND_HELP_TEXT:
.asciiz "Colour bars"
.asciiz "Clear Screen"
.asciiz "Display ram"
.asciiz "Execute program"
.asciiz "Fill mem"
.asciiz "Jump"
.asciiz "S-Rec loader"
.asciiz "Mem test"
.asciiz "Read mem"
.asciiz "Sleep"
.asciiz "Switch to ROM"
.asciiz "Set Video mode"
.asciiz "Write mem"
.asciiz "Help"
.byte $00


T_MOTD:             .BYTE "LIV2 ",$FF,$0C,"6",$FF,$0A,"5",$FF,$09,"C",$FF,$0A,"02 Monitor ROM",$00
T_BRK:             .asciiz "BRK AT $"
T_ADDR:         .asciiz "Addrs"
T_START:             .asciiz "Start Addr: $"
T_JUMP:         .asciiz "Jump to: $"
T_SRECWAIT:     .BYTE "Waiting for S-Record File...",$0A,$0D,$00
T_SCHKSUMERR:   .asciiz "CHECKSUM FAILED FOR: $"
T_LOADED:       .BYTE "Loading complete.",$0A,$0D,$00
T_LOADING:      .asciiz "Loading "
T_UNKNOWN:      .asciiz "Invalid command"
T_BRKPROMPT:    .asciiz "[C]ontinue/[R]eset/Jump to [M]onitor?"
T_AXYP:         .BYTE "YXAP"

STR_TABLE:
.WORD   T_MOTD ; STRING 0
.WORD   T_BRK
.WORD   T_ADDR
.WORD   T_START
.WORD   T_JUMP
.WORD   T_SRECWAIT
.WORD   T_SCHKSUMERR
.WORD   T_LOADED
.WORD   T_LOADING
.WORD   T_UNKNOWN
.WORD   T_BRKPROMPT

.SEGMENT "VECTORS" ; 6502 VECTORS
            .WORD IRQ
            .WORD COLDSTRT
            .WORD IRQ
