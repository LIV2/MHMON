.scope ERR
	EOK          = 0
	ENOENT       = 1
	EIO          = 2
	ENOMEM       = 3
	ENODEV       = 4
	ENODISK      = 5
	EFILENAME    = 6
      EFSNOSUPPORT = 7
      ESYNTAX      = 8
      ENOEXEC      = 9
.endscope

.scope ERRMSG
PRINT:      PHX
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

EOK:          .asciiz "No error."
ENOENT:       .asciiz "No such file or directory."
EIO:          .asciiz "I/O Error."
ENOMEM:       .asciiz "Out of memory."
ENODEV:       .asciiz "No such device."
ENODISK:      .asciiz "No disk inserted."
EFILENAME:    .asciiz "Bad filename."
EFSNOSUPPORT: .asciiz "FS not supported."
ESYNTAX:      .asciiz "Syntax error."
ENOEXEC:      .asciiz "Not an executable."

ERRORMSG_TABLE:
.WORD   EOK
.WORD   ENOENT
.WORD   EIO
.WORD   ENOMEM
.WORD   ENODEV
.WORD   ENODISK
.WORD   EFILENAME
.WORD   EFSNOSUPPORT
.WORD   ESYNTAX
.WORD   ENOEXEC
.endscope
