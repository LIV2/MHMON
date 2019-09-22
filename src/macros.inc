.macro debugmsg msg, var_a, var_b
	.ifdef debug
            .if debug=1
		    printmsg msg, var_a, var_b
	     .endif
      .endif
.endmacro

.macro delay time
.local @LOOP
       PHX
       LDX #time
@LOOP: DEX
       BNE @LOOP
       PLX
.endmacro

.macro printmsg msg, var_a, var_b
	PHA
	JSR PRIMM
	.asciiz msg
      .ifnblank var_a
            LDA var_a
            JSR HEXTOASC
      .endif
      .ifnblank var_b
            LDA var_b
            JSR HEXTOASC
      .endif
      JSR CRLF
	PLA
.endmacro

.macro      m_print32   addr
            PHA
            LDA addr+3
            JSR HEXTOASC
            LDA addr+2
            JSR HEXTOASC
            LDA addr+1
            JSR HEXTOASC
            LDA addr
            JSR HEXTOASC
            PLA
.endmacro

.macro      m_print16   addr
            PHA
            LDA addr+1
            JSR HEXTOASC
            LDA addr
            JSR HEXTOASC
            PLA
.endmacro