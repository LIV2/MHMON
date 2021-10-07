;FD.inc
;debug=1

.struct FD
    FLAGS        .byte
    DEVICE       .byte
    CLUSTERSIZE  .byte
    FIRSTCLUSTER .word
    CURCLUSTER   .word
    NEXTCLUSTER  .word
    SEEKPOS      .word
.endstruct

; FDZERO zero out FD memory
FDZERO:     LDX #$00
@LOOP:      STZ FDS,X
            INX
            BNE @LOOP
            RTS


; FOPENPTR filename
FOPEN:      LDA (FD_FILENAMEPTR)
            SEC
            SBC #'A'
            CMP #$02 ; todo support more drives...
            BCS @ERR
            STA FDCDRV
            PHY
            LDY #$01
            LDA (FD_FILENAMEPTR),Y
            PLY
            CMP #':'
            BNE @ERR
            CLC
            LDA FD_FILENAMEPTR
            ADC #$02
            STA FAT_FILENAMEPTR
            LDA FD_FILENAMEPTR+1
            ADC #$00
            STA FAT_FILENAMEPTR+1
            JSR FNPARSE
            BCS @ERR+2
            JSR FATINIT
            BCS @ERR+2
            JSR FAT_FIND
            BCS @ERR+2
            JSR FDALLOC
            BCS @ERR+2
            LDA FDCDRV
            STA FDS+FD::DEVICE,X
            LDA FAT_LSPC
            CMP #$01 ; Throw an error for LSPC > 1 for now until I fix support
            BEQ @SUPPORTED
            LDA #ERR::ELSPC
            RTS
@SUPPORTED: STA FDS+FD::CLUSTERSIZE,X
            LDA FAT_CLUSTERNUM+1
            STA FDS+FD::FIRSTCLUSTER+1,X
            STA FDS+FD::NEXTCLUSTER+1,X
            LDA FAT_CLUSTERNUM
            STA FDS+FD::FIRSTCLUSTER,X
            STA FDS+FD::NEXTCLUSTER,X
            STZ FDS+FD::SEEKPOS,X
            STZ FDS+FD::SEEKPOS+1,X
            JSR FDCACHE
            BCS @ERR+2
            RTS
@ERR:       LDA #ERR::ENOENT
            SEC
            RTS

FDCACHE:    PHX
            TXA
            LSR A
            LSR A
            LSR A
            CLC
            ADC #$C0
            STA FATSECTORDEST+1
            STZ FATSECTORDEST
            LDA FDS+FD::NEXTCLUSTER+1,X
            STA FDS+FD::CURCLUSTER+1,X
            STA FAT_CLUSTERNUM+1
            LDA FDS+FD::NEXTCLUSTER,X
            STA FDS+FD::CURCLUSTER+1,X
            STA FAT_CLUSTERNUM
            LDA FDS+FD::DEVICE,X
            STA FDCDRV
            JSR FAT_GETCLUSTER
            BCS @END
            LDA FAT_CLUSTERNUM+1
            STA FDS+FD::NEXTCLUSTER+1,X
            LDA FAT_CLUSTERNUM
            STA FDS+FD::NEXTCLUSTER,X
@END:       PLX
            RTS

FDALLOC:    LDX #$00
@FINDFREE:  LDA FDS,X
            BEQ @FOUNDFREE
            TXA
            CLC
            ADC #16
            TAX
            BPL @FINDFREE
            LDA #ERR::ENOMEM
            SEC
            RTS
@FOUNDFREE: LDA #$01
            STA FDS,X
            TXA
            CLC
            RTS


FREAD:      LDA FDS,X
            BEQ @ERR
            TXA
            LSR A
            LSR A
            LSR A
            CLC
            ADC #$C0
            STA FDPTR+1
            LDA FDS+FD::SEEKPOS,X
            STA FDPTR
            LDA FDS+FD::SEEKPOS+1,X
            AND #$01
            CLC
            ADC FDPTR+1
            STA FDPTR+1
            LDA (FDPTR)
            PHA
            INC FDS+FD::SEEKPOS,X
            BNE @NOINCH
            LDA FDS+FD::SEEKPOS+1,X
            INC A
            STA FDS+FD::SEEKPOS+1,X
            AND #$01
            BNE @NOINCH
            JSR FDCACHE
            BCS @ERR-1
@NOINCH:    PLA
            CLC
            RTS

            PLA
@ERR:       ;LDA #ERR::ENOMEM
            SEC
            RTS

FDFREE:     CPX #$08
            BCS @ERR
            STZ FDS,X
            CLC
            RTS
@ERR:       LDA #ERR::ENOMEM
            SEC
            RTS