.pc02

;debug=1
duart=1
video=1
floppy=1
;acia=1
via=1

.include "constants.inc"
.include "macros.inc"


.SEGMENT "ROMCODE"

; Pointers to functions so that ROM changes don't break programs
V_CHARIN:   JMP CHARIN      ; Char in
V_CHAROUT:  JMP CHAROUT     ; Char out
V_CRLF:     JMP CRLF        ; Print CRLF
V_PROMPT:   JMP PROMPT      ; LOAD (do nothing)
V_SAVE:     JMP SAVE        ; SAVE (do nothing)
V_CLS:      JMP CLS         ; Clear screen
V_HEXTOASC: JMP HEXTOASC    ; HEX byte to Ascii
V_CHARINW:  JMP CHARINW     ; Char in (wait)
V_HEXOUT:   JMP HEXOUT      ; Hex out
V_CHARINU:  JMP CHARINU     ; Char in uppercase
.ifdef floppy
V_FOPEN:    JMP FOPEN
V_FCLOSE:   JMP FDFREE
V_FREAD:    JMP FREAD
.else
; No file support compiled, do nothing
; Add NOPs so that the vector alignment is preserved
V_FOPEN:    NOP
            NOP
            RTS
V_FCLOSE:   NOP
            NOP
            RTS
V_FREAD:    NOP
            NOP
            RTS
.endif
V_ERRMSG:   JMP ERRPRINT
; **** COLD START ENTRY ****
; Set up interrupts, stack, hardware and drop through to the promp
COLDSTRT:   SEI                   ;   Disable interrupts
            LDA #<IRQ_ROM         ;   Set softVector to default ROM IRQ handler
            STA IRQ_softVector    ;   softVector allows inserting additional IRQ handlers before ROM handlers
            LDA #>IRQ_ROM
            STA IRQ_softVector+1
            LDX #$FF              ;   Initialize the Stack Pointer
            TXS
            STZ SECONDSL
            STZ SECONDSH
            STZ JIFFIES
            STZ INPBUFHEAD
            STZ INPBUFTAIL
            STZ CMDBUFHEAD
            STZ CMDBUFPTR
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
.ifdef floppy
INITFLOPPY: JSR FDZERO
            JSR FDCINIT
            BCC MAIN
            printmsg "FDC ERROR"
.endif
MAIN:       LDA #$00   ; Print boot message
            JSR STROUT
            JMP PROMPT
; **** END OF COLD START ****
;
; **** Public routines ****

; CHARIN - Get a character from INPBUF (if there is one)
; Returns Character in A with carry set, carry clear if INBUF is empty
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

; CHARINW - CHARIN but blocking until a character is ready
CHARINW:    JSR CHARIN
            BCC CHARINW
            RTS

; CHARINU - CHARIN but force uppercase characters
CHARINU:    JSR CHARIN
            BCC @END+1
            CMP #'a'
            BCC @END
            CMP #'z'+1
            BCS @END
            EOR #$20
@END:       SEC
            RTS

; CHAROUT - Send a character to the screen/terminal
; Parameters: A - Character
CHAROUT:
.ifdef video
            PHA
            JSR COUTVIDEO     ; Output the character to the screen
            PLA
.endif
CHAROUTS:   CMP #$7F          ; Change DEL to Backspace
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
            LDA DUARTSRA      ; Does the buffer have room for our character?
            AND #$04
            BEQ DUARTTX+1     ; If not, loop until there is room
            PLA
            STA DUARTTXA      ; Send the character to the DUART
.endif
            RTS

; CLS: Clear the screen
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

; CRLF - Print a Carriage return followed by a Linefeed
CRLF:       LDA #$0A
            JSR CHAROUT
            LDA #$0D
            JSR CHAROUT
            RTS

; HEXTOASC - Print a byte as Hex
; Parameters: A - Byte to print
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

; HEXOUT - Print a digit in Hex
; Parameters: A - nibble to print as hex
HEXOUT:     PHA
            CLC
            CMP #$0A
            BCC HEXL        ;   If 0-9, Add 0x30 to get Ascii code
            ADC #$06        ;   If A-F, Add 0x36 (plus carry!) to get the proper ascii code
HEXL:       ADC #$30
            JSR CHAROUT
            PLA
            RTS


; PROMPT - Command prompt
; Programs returning to the monitor on exit should jump here via V_PROMPT
PROMPT:     JSR CRLF
            LDA #']'
            JSR CHAROUT
            LDX #$00
            STZ CMDBUFPTR     ; Reset CMDBUF to start a new command
            STZ CMDBUF,X
@PROMPTL:   JSR CHARINU
            BCC @PROMPTL
            CMP #$0D          ; Enter?
            BEQ @GO           ; Go run command
            CMP #$7F          ; Delete/Backspace?
            BEQ @BS
            CMP #$08
            BNE @STORE
@BS:        DEX               ; Decrement CMDBUFPTR (in X)
            BPL @NOERR        ; If we reached the beginning of the line
            LDA #$07          ; Send BELL to the terminal
            JSR CHAROUT
            INX               ; Move CMDBUFPTR back to 0
            BRA @PROMPTL      ; Go back to waiting for a character
@NOERR:     JSR CHAROUT       ; Send the Backspace to the terminal (move the cursor back 1 character)
            PHA
            LDA #' '          ; Rub out the character that was there (moves the cursor forward)
            JSR CHAROUT       ;
            PLA
            JSR CHAROUT       ; Move the cursor back again
            STZ CMDBUF,X      ; Terminate the CMDBUF string here
            BRA @PROMPTL      ; Go back to waiting for input

@STORE:     CMP #' '          ; Check if character is printable (< Space)
            BCC @PROMPTL      ; If not, ignore it
            CMP #$7F          ; >= DEL
            BCS @PROMPTL      ; Ignore it
            JSR CHAROUT       ; Print it
            STA CMDBUF,X      ; Store it in CMDBUF
            INX               ; Increment CMDBUFPTR
            BPL @PROMPTL      ; Bounds check of CMDBUF's 127 byte buffer. Branch unless full
            DEX               ; Decrement CMDBUFPTR to keep it within range
            STZ CMDBUF,X      ; Terminate the CMDBUFPTR string at 127 bytes to prevent overflow on read
            LDA #$07
            JSR CHAROUT       ; Signal the full buffer with BELL
            LDA #$7F          ; Delete the last character printed
            JSR CHAROUT
            BRA @PROMPTL      ; Continue reading input
; GO - Enter was pressed, find the command in the CMDTAB and if found, run it
@GO:        STZ CMDBUF,X      ; Terminate the CMDBUF string
            LDX #$FF
@SEARCH:    INX
            LDA T_COMMAND_TAB,X
            BEQ UNKNOWN       ; Hit NULL while searching T_COMMAND_TAB, do syntax e
            CMP CMDBUF        ; Compare the 1st character in CMDBUF with COMMAND_TAB_X
            BNE @SEARCH       ; If not matched, keep searching
            TXA
            ASL A
            TAX               ; Put the command offset in T_COMMAND_ADDR into X
            LDA #$02
            STA CMDBUFPTR     ; Point CMDBUFPTR to the command args
            JSR CRLF
            LDA CMDBUF+1      ; Load char 2 of CMDBUF into A
                              ; Useful for syntax checking in the command, will be 0 if no args 
            JMP (T_COMMAND_ADDR,X)
@ERROR:     LDA #ERR::ESYNTAX
            JSR ERRPRINT
            JMP PROMPT
; UNKNOWN - Print "Invalid command"
UNKNOWN:    JSR CRLF
            JSR CRLF
            LDA #$09
            JSR STROUT
            JMP PROMPT

; **** Private routines ****

; HEXGET - Parse a hex number from CMDBUF
; Parameters: CMDBUF - Byte in hex notation
; Returns: Byte in A, Carry clear on error
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

; BINGET - Parse a binary number from CMDBUF to a byte
; Parameters: Binary string in CMDBUF
; Returns: A - Byte, Carry - Set on error
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

; BINPUT - Print a byte in binary notation
; Parameters: A - byte to print
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

; STROUT - Print a string from STR_TABLE indexed by A
; Parameters: A - String index
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
; PRIMM
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


; **** Commands ****
.ifdef floppy
TST_FOPEN:  CMP #' '
            BNE @STXERR
            LDA #<CMDBUF+2
            STA FD_FILENAMEPTR
            LDA #>CMDBUF
            STA FD_FILENAMEPTR+1
            JSR FOPEN
            BCS @ERROR
            JSR PRIMM
            .ASCIIZ "Opened with FD #"
            TXA
            JSR HEXTOASC
            JSR CRLF
            ;JSR FDFREE
@END:       JMP PROMPT
@STXERR:    LDA #ERR::ESYNTAX
@ERROR:     CMP #$00 ; EOF?
            BEQ @END
            JSR ERRPRINT
            JMP PROMPT
.endif

; BARS: prints a color bar test pattern on the display
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

; Sleep
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

; Fill memory
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

; DISPLAYRAM
; Displays memory contents at the address specified
; Parameters: <start address> [end address]
DISPLEND2:  JMP SYNTAXERR
READ16:     LDA STARTL
            SEC
            ADC #$FF
            STA ENDL
            LDA STARTH
            ADC #$00
            STA ENDH
            BRA GO_DISPLAY
; We start here
DISPLAYRAM: CMP #' '
            BNE DISPLEND2     ; It doesn't appear we got a start address, Syntax error
            JSR HEXGET        ; Get start address high byte
            BCC DISPLEND2     ; Syntax error?
            STA STARTH        ; Store start address high
            JSR HEXGET        ; Get start address low
            BCC DISPLEND2     ; Syntax error?
            AND #$F0          ; Zero out the lower nibble, so we always display at least 16 bytes
            STA STARTL        ; Store low byte
            LDX CMDBUFPTR
            LDA CMDBUF,X
            CMP #' '          ; Have we been supplied with an end address?
            BNE READ16        ; If not, display 256 bytes by default
            INX
            STX CMDBUFPTR
            JSR HEXGET        ; Get End high address
            BCC DISPLEND2     ; Syntax error?
            STA ENDH          ; Store high address
            JSR HEXGET        ; Get Low end address
            BCC DISPLEND2     ; Syntax error?
            ORA #$0F          ; Always display in rows of 16 bytes
            INC
            BNE @NOCARRY
            INC ENDH
@NOCARRY:   STA ENDL
; Draw the header
GO_DISPLAY: JSR CRLF
            LDA #$02
            JSR STROUT        ; Print "Addr: "
            LDA #$09          ; TAB
            JSR CHAROUT
            LDY #$00
DISPLOOP1:  TYA               ; Loop1: Display Lower Address Nibble
            JSR HEXTOASC
            LDA #' '
            JSR CHAROUT
            INY
            CPY #$10
            BNE DISPLOOP1
            JSR CRLF
            LDA #'-'          ; Loop2: Draw horizontal rule
            LDY #$37          ;
DISPLOOP2:  JSR CHAROUT       ;
            DEY               ;
            BNE DISPLOOP2     ;
DISPLINE:   LDY #$00          ; Display Bytes at START,Y
            JSR CRLF
            LDA #'$'          ; Display Address, i.e "$0010"
            JSR CHAROUT       ;
            LDA STARTH        ;
            JSR HEXTOASC      ;
            LDA STARTL        ;
            JSR HEXTOASC      ;
            LDA #$09          ; TAB
            JSR CHAROUT       ;
DISPLINE1:  LDA (STARTL),Y    ; Display 16 Bytes 0-F for each line
            JSR HEXTOASC      ;
            LDA #' '          ;
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
@NOMATCH:   INY
            CPY #$10                ;
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
            JSR ERRPRINT
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

; Jump: jump to the specified address
JUMP:       CMP #' '
            BNE @SYNTAX
            LDA #$04
            JSR STROUT
            JSR HEXGET
            BCC @EXIT
            STA STARTH
            JSR HEXGET
            BCC @EXIT
            STA STARTL
            JSR CRLF
            JSR CRLF
            JMP (STARTL)
@EXIT:      JMP PROMPT
@SYNTAX:    LDA #ERR::ESYNTAX
            JSR ERRPRINT
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

; C_CLS: CLS wrapped in a command
;
C_CLS:      BEQ @SYNTAXOK
            JMP SYNTAXERR
@SYNTAXOK:  JSR CLS
            JMP PROMPT


SETMODE:    BNE SYNTAXERR
            JSR VIDMODE
            JMP PROMPT

SYNTAXERR:  LDA #ERR::ESYNTAX
            JSR ERRPRINT
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

.ifdef floppy
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
            JSR ERRPRINT
            JMP PROMPT
@END:       JSR FDFREE
            CLC
            JMP (ENDL)
@NOTEXEC:   LDA #ERR::ENOEXEC
            BRA @ERROR
.endif

;## Motorola S-Record Loader
; Supports the following record types
; S0 - Header text
; S1 - Data (16-bit Address)
; S9 - Start Address
;
SREC:       BEQ @SYNTAXOK
            LDA #ERR::ESYNTAX
            JSR ERRPRINT
            JMP PROMPT
@SYNTAXOK:  JSR CRLF
            LDA #$05     ; Print "Waiting for S-Record File..."
            JSR STROUT
@START:     JSR CHARINU  ; Read first byte of record
            BCC @START
            CMP #$1B     ; Exit if escape pressed
            BNE @GO
            JMP @END
@GO:        CMP #'S'
            BNE @START   ; Not an S record as it didn't start with 'S' ignore
@START1:    JSR CHARINU
            BCC @START1
            CMP #$1B     ; Exit on Escape
            BEQ @END
            CMP #'0'     ; S0 record?
            BNE @S9
            JMP @MESSAGE ; Go handle S0 Record
@S9:        CMP #'9'     ; S9 Record?
            BNE @S1
            JMP @EXECUTE ; Go handle S9 record
@S1:        CMP #'1'     ; S1 record?
            BNE @IGNORE  ; No,
            LDA #'#'
            JSR CHAROUT
            JSR HEXINS   ; Byte Count
            BCS @END
            STA CHECKSUM
            TAX          ; Byte count now in X
            DEX
            JSR HEXINS   ; Address High Byte
            BCS @END
            STA STARTH
            DEX
            CLC
            ADC CHECKSUM
            CLC
            STA CHECKSUM
            JSR HEXINS   ; Address Low Byte
            STA STARTL
            DEX
            CLC
            ADC CHECKSUM
            CLC
            STA CHECKSUM
            LDY #$00
@DLOOP:     JSR HEXINS    ; Get remaining bytes and store them in memory
            STA (STARTL),Y
            CLC
            ADC CHECKSUM
            CLC           ; Don't care about the carry
            STA CHECKSUM
            INY
            DEX
            BNE @DLOOP
            LDA #$FF
            EOR CHECKSUM  ; One's complement Checksum
            STA CHECKSUM
            JSR HEXINS    ; Senders Checksum
            CMP CHECKSUM  ; Compare Senders checksum with our own
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
; Pay no attention to non S0,S1,S9 records
@IGNORE:    JSR HEXINS
            BCS @END
            TAX
@IGLOOP:    JSR HEXINS
            DEX
            BNE @IGLOOP
            JMP @START
; S9 Record aka Start Address, jump to the address specified by this record
@EXECUTE:   JSR HEXINS  ; Get Length (but we already know it's 3 bytes long so don't keep the result)
            JSR CRLF
            LDA #$04    ; Print "Jump to: $"
            JSR STROUT
            JSR HEXINS   ; Get High byte of start address
            STA STARTH
            JSR HEXOUT
            JSR HEXINS   ; Get Low byte of start address
            STA STARTL
            JSR HEXOUT
            JSR HEXINS  ; Get checksum (but don't bother checking it)
            JSR CRLF
            JMP (STARTL)
; S0 Record (ASCII Text) - Print the message contained within
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
; CTSCONTROL
CTSCONTROL: PHA
            JSR INPBUFSZ ; Buffer full?
            CPX #$7F 
            BCC @ENABLE  ; No, Terminal is Clear to send
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
.ifdef floppy
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
.endif
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

SAVE: RTS


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

; ERRPRINT: get the string for the error number and print it
; Parameters: A - Error number
ERRPRINT:   PHX
            PHY
            PHA
            PLA
            ASL A
            TAX
            LDA ERRORMSG_TABLE,X
            STA STRLO
            LDA ERRORMSG_TABLE+1,X
            STA STRHI
            LDY #$00
@LOOP:      LDA (STRLO),Y
            BEQ @END
            JSR CHAROUT
            INY
            JMP @LOOP
@END:       JSR CRLF
            PLY
            PLX
            RTS

.include "errors.inc"
.include "math.inc"
.ifdef floppy
.include "disk.inc"
.include "fat.inc"
.include "FD.inc"
.endif
.include "video.inc"

.SEGMENT "ROMDATA"
T_COMMAND_TAB:
.byte 'B'
.byte 'C'
.byte 'D'
.ifdef floppy
.byte 'E'
.endif
.byte 'F'
.byte 'J'
.byte 'L'
.byte 'M'
.byte 'R'
.byte 'S'
.ifdef floppy
.byte 'T'
.endif
.byte 'U'
.byte 'V'
.byte 'W'
.byte '?'
.byte $00

T_COMMAND_ADDR:
 .WORD BARS
 .WORD C_CLS
 .WORD DISPLAYRAM
 .ifdef floppy
 .WORD LOADFILE
 .endif
 .WORD FILL
 .WORD JUMP
 .WORD SREC
 .WORD MEMTEST
 .WORD READMEM
 .WORD SLEEP
 .ifdef floppy
 .WORD TST_FOPEN
 .endif
 .WORD UNLOAD
 .WORD SETMODE
 .WORD WRITEMEM
 .WORD HELP
 .WORD PROMPT

T_COMMAND_HELP_TEXT:
.asciiz "Colour bars"
.asciiz "Clear Screen"
.asciiz "Display ram"
.ifdef floppy
.asciiz "Execute program"
.endif
.asciiz "Fill mem"
.asciiz "Jump"
.asciiz "S-Rec loader"
.asciiz "Mem test"
.asciiz "Read mem"
.asciiz "Sleep"
.ifdef floppy
.asciiz "Test FOPEN"
.endif
.asciiz "Switch to ROM"
.asciiz "Set Video mode"
.asciiz "Write mem"
.asciiz "Help"
.byte $00


T_MOTD:         .BYTE "LIV2 ",$FF,$0C,"6",$FF,$0A,"5",$FF,$09,"C",$FF,$0A,"02 Monitor ROM - "
.incbin "obj/builddate.txt"
.BYTE $00
T_BRK:          .asciiz "BRK AT $"
T_ADDR:         .asciiz "Addrs"
T_START:        .asciiz "Start Addr: $"
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
