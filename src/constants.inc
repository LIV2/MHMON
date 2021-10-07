;**** Hardware registers ****
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

;**** Zero Page vars ****
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

;**** Everything else ****
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
FDS             = $0900
FATBUF          = $0C00
SECTORBUF       = $0A00

.ifdef floppy
;;; FAT Vars
FAT_ROOTSECTS    = FDCVAREND+1
FAT_BPLS         = FAT_ROOTSECTS+4
FAT_LSPC         = FAT_BPLS+4
FAT_RESERVED     = FAT_LSPC+4
FAT_ROOTSTART    = FAT_RESERVED+4
FAT_DATASTART    = FAT_ROOTSTART+4
FAT_DATASECTS    = FAT_DATASTART+4
FAT_DATACLUSTERS = FAT_DATASECTS+4
FAT_TYPE         = FAT_DATACLUSTERS+4
FAT_MEDDESC      = FAT_TYPE+1
FAT_CLUSTERNUM   = FAT_TYPE+1
FAT_ENTOFFSET    = FAT_CLUSTERNUM+4
FAT_CACHED       = FAT_ENTOFFSET+2
FAT_SIZE         = FAT_CACHED+4
FAT_FILENAME     = FAT_SIZE+4
.endif
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
      ELSPC        = 10
.endscope
