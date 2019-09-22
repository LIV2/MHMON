;VIDEO

VGAREGPTR   = $DF00
VGAREG      = $DF01
PROMPTCOLOR = $0A
.scope VGA
VRAMTOP  = $A000
VRAMEND  = $BFFF
SCROLL_L = $00 ; Scroll offset (low byte)
SCROLL_H = $01 ; Scroll offset (high byte)
CONFIG   = $02 ; Config register:
               ;     0x80: CGROM Bank select (8x8 mode only)
               ;     0x40: Cursor enable
               ;     0x20: Cursor flash enable
               ;     0x10: Mode (0: 640x480 8x16 characters, 1: 640x400 8x8 characters)
               ;     0x00-0x08: Character height
CURS_L   = $03 ; Cursor position (low byte)
CURS_H   = $04 ; Cursor position (high byte)
.endscope

VIDINIT:    LDA #$B2
            STA VIDPTRH
            STA CHARPTRH
            LDA #$20
            STA VIDPTRL
            STA CHARPTRL
            LDA #$01
            STA CREG
            STZ CURSX
            STZ SCROLLLO
            STZ SCROLLHI
            STZ VGAREGPTR       ; Set scroll register to $0000
            STZ VGAREG          ;
            LDA #VGA::SCROLL_H  ;
            STA VGAREGPTR       ;
            STZ VGAREG          ;
            LDA #VGA::CONFIG    ; Configure Mode
            STA VGAREGPTR       ;
            LDA #$6F            ; Mode 0, Cursor enabled & flashing, char height 16 pixels
            STA VGAREG
            LDA #VGA::CURS_L    ; Set cursor to $0910 (line 29)
            STA VGAREGPTR
            LDA #$10
            STA VGAREG
            LDA #VGA::CURS_H
            STA VGAREGPTR
            LDA #$09
            STA VGAREG
            LDA #PROMPTCOLOR
            STA COLOR
            JSR CLS
            RTS

CHARPTR:    PHA
            PHY
            LDA VIDPTRL
            CLC
            ADC CURSX
            STA CHARPTRL
            LDA VIDPTRH
            ADC #$00
            CMP #>VGA::VRAMEND+1
            BCC @NOWRAP
            LDA #>VGA::VRAMTOP
@NOWRAP:    STA CHARPTRH
            ROR
            LDY #VGA::CURS_H
            STY VGAREGPTR
            STA VGAREG
            DEY
            STY VGAREGPTR
            LDA CHARPTRL
            ROR
            STA VGAREG
@NOWRAP2:   PLY
            PLA
            RTS

COUTVIDEO:  BBR0 CREG,@EXIT2
            PHY
            LDY CURSX
            CMP #$0D
            BNE @LF
            STZ CURSX
            BRA @EXIT
@LF:        CMP #$0A
            BNE @TAB
            JSR SCROLL
            BRA @EXIT
@TAB:       CMP #$09
            BNE @NOTTAB
            INY
            INY
            INY
            INY
            BRA @INCPTR
@NOTTAB:    CMP #$08
            BEQ @BS
            CMP #$7F
            BNE @NOTBS
@BS:        CPY #$02
            BCC @INCPTR+2
            DEY
            DEY
            BRA @INCPTR+2
@NOTBS:     CMP #' '
            BCC @EXIT+3
            CPY #$9F       ; Wrapped around to new line?
            BCC @STORE
            STZ CURSX      ; Yes, move cursor to column 0
            JSR SCROLL     ; Scroll screen + clear new line
            LDY #$00
@STORE:     STA (CHARPTRL)
            LDA COLOR
            PHY
            LDY #$01
            STA (CHARPTRL),Y
            PLY
@INCPTR:    INY
            INY
            STY CURSX
@EXIT:      JSR CHARPTR
            PLY
@EXIT2:     RTS

PLOT:       PHA
            PHY
            PHX
            TXA
            ASL A
            STA CURSX
            LDA SCROLLLO
            STA VIDPTRL
            LDA SCROLLHI
            CLC ;;
            ADC #$50
            STA VIDPTRH
            LDA VIDPTRL
            INY
@YLOOP:     LDA VIDPTRL
            DEY
            BEQ @DONE
            CLC ;;
            ADC #$50
            STA VIDPTRL
            LDA VIDPTRH
            ADC #$00
            STA VIDPTRH
            BRA @YLOOP
@DONE:      STA VIDPTRL
            ASL VIDPTRL
            ROL VIDPTRH
            JSR CHARPTR
@END:       PLX
            PLY
            PLA
            RTS

SCROLL:     PHA
            PHY
            LDY #$00
            STY VGAREGPTR
            LDA SCROLLLO
            CLC
            ADC #$50
            STA SCROLLLO
            STA VGAREG
            BCC @INCVPTR
            INY
            STY VGAREGPTR
            LDA SCROLLHI
            INC
            STA SCROLLHI
            STA VGAREG

; Wrap-around at $C000
@INCVPTR:   LDA VIDPTRL
            CLC
            ADC #$A0
            STA VIDPTRL
            LDA VIDPTRH
            ADC #$00
            STA VIDPTRH
            CMP #>VGA::VRAMEND+1
            BCC @NEXT2
            LDA #>VGA::VRAMTOP
            STA VIDPTRH
@NEXT2:
@CLINE:     LDY #$50      ; Clear the new line
            LDA VIDPTRL
            PHA
            LDA VIDPTRH
            PHA
@LOOP:      LDA #$00
            STA (VIDPTRL)
            INC VIDPTRL
            LDA COLOR
            STA (VIDPTRL)
            INC VIDPTRL
            BNE @NOLWRAP
            LDA VIDPTRH
            INC A
            CMP #>VGA::VRAMEND+1
            BCC @NOHWRAP
            LDA #>VGA::VRAMTOP
@NOHWRAP:   STA VIDPTRH
@NOLWRAP:   DEY
            BNE @LOOP
            PLA
            STA VIDPTRH
            PLA
            STA VIDPTRL
            PLY
            PLA
            RTS

.ifdef video
VIDMODE: 
            LDA #$6F
            BBR1 CREG,@SKIP
            PHA
            LDA #$BE
            STA VIDPTRH
            LDA #>VGA::VRAMTOP
            STA VIDPTRL
            JSR CHARPTR
            LDA #VGA::SCROLL_L
            STA VGAREGPTR
            STZ VGAREG
            STZ SCROLLLO
            LDA #VGA::SCROLL_H
            STA VGAREGPTR
            STZ VGAREG
            STZ SCROLLHI
            STZ CURSX
            PLA
            CLC
            ADC #$08
            BBR2 CREG,@SKIP
            ORA #$80
@SKIP:      PHA
            LDA #VGA::CONFIG
            STA VGAREGPTR
            PLA
            STA VGAREG
            RTS
.else
VIDMODE:    RTS
.endif