

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
ELSPC:        .asciiz "Unsupported LSPC Value :("

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
.WORD   ELSPC
