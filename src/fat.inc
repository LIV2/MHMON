; FAT
;debug=1
PTR_VBR_OEM          = SECTORBUF+$03 ; OEM Name
PTR_BPB_BPLS         = SECTORBUF+$0B ; Bytes per logical sector
PTR_BPB_LSPC         = SECTORBUF+$0D ; Logical sectors per cluster
PTR_BPB_RESERVED     = SECTORBUF+$0E ; Number of reserved sectors
PTR_BPB_NUMFATS      = SECTORBUF+$10 ; Number of FATs
PTR_BPB_NUMROOT      = SECTORBUF+$11 ; Max number of root directory entries
PTR_BPB_LOGSECTS     = SECTORBUF+$13 ; Total logical sectors
PTR_BPB_MEDDESC      = SECTORBUF+$15 ; Media descriptor
PTR_BPB_LOGSECPERFAT = SECTORBUF+$16 ; Logical sectors per FAT
PTR_BPB_VOLLABEL     = SECTORBUF+$2B ; Volume Label

;;; FAT Vars
FAT_ROOTSECTS    = FDCDRV+1
FAT_BPLS         = FAT_ROOTSECTS+4
FAT_LSPC         = FAT_BPLS+4
FAT_RESERVED     = FAT_LSPC+4
FAT_ROOTSTART    = FAT_RESERVED+4
FAT_DATASTART    = FAT_ROOTSTART+4
FAT_DATASECTS    = FAT_DATASTART+4
FAT_DATACLUSTERS = FAT_DATASECTS+4
FAT_TYPE         = FAT_DATACLUSTERS+4
FAT_CLUSTERNUM   = FAT_TYPE+1
FAT_ENTOFFSET    = FAT_CLUSTERNUM+4
FAT_CACHED       = FAT_ENTOFFSET+2
FAT_SIZE         = FAT_CACHED+4
FAT_FILENAME     = FAT_SIZE+4

;#######
;# FAT #
;#######

; FNPARSE
; Check filename in (FAT_FILENAMEPTR) and if valid, copy to FAT_FILENAME
;
FNPARSE:    LDX #$00
            LDY #$00
@LOOP:      LDA (FAT_FILENAMEPTR),Y
            BEQ @LOOPEND
            CMP #'0'
            BCC @NOTNUM
            CMP #'9'+1
            BCC @NEXT
@NOTNUM:    CMP #'@'
            BCC @NOTALPHA
            CMP #'Z'+1
            BCC @NEXT
@NOTALPHA:  CMP #'`'
            BEQ @ERROR
            CMP #' '
            BCC @ERROR
            BEQ @NEXT
            CMP #'/'
            BCS @ERROR
            CMP #'*'
            BEQ @ERROR
            CMP #'+'
            BEQ @ERROR
            CMP #','
            BEQ @ERROR
            CMP #'.'
            BNE @ERROR
            INX
@NEXT:      INY
            CPY #11
            BNE @LOOP
@LOOPEND:   CPX #$01
            BNE @ERROR
            LDY #11
@LOOP2:     LDA #' '
            STA FAT_FILENAME,Y
            DEY
            BPL @LOOP2
            LDY #$00
            LDX #09
@LOOP3:     LDA (FAT_FILENAMEPTR),Y
            CMP #'.'
            BEQ @JUMPTOEXT
            STA FAT_FILENAME,Y
            INY
            DEX
            BNE @LOOP3
            BRA @ERROR ; Extension missing?
@JUMPTOEXT: INY
@EXT:       LDX #08
@EXTLOOP:   LDA (FAT_FILENAMEPTR),Y
            STA FAT_FILENAME,X
            INX
            INY
            CPX #11
            BNE @EXTLOOP
            CLC
            RTS
@ERROR:     SEC
            LDA #ERR::EFILENAME
            RTS

; FATINIT
; Initialize FAT driver
;
FATINIT:    LDA #$FF
            STA FAT_CACHED
            STA FAT_CACHED+1
            STZ NEXTSECTOR
            STZ NEXTSECTOR+1
            LDA #<SECTORBUF
            STA SECTORBUFPTR
            STA FATSECTORBUFPTR
            LDA #>SECTORBUF
            STA SECTORBUFPTR+1
            STA FATSECTORBUFPTR+1
            JSR FDCREADLBA
            BCC @OK
            RTS
@OK:        STZ FAT_TYPE
            LDA PTR_BPB_LOGSECTS
            ORA PTR_BPB_LOGSECTS+1
            BNE @SUPPORTED
            LDA #ERR::EIO
            SEC
            RTS
@SUPPORTED: m_copy16 PTR_BPB_BPLS, FAT_BPLS
            m_copy16 PTR_BPB_RESERVED, FAT_RESERVED
            m_copy16 PTR_BPB_LOGSECPERFAT, FAT_SIZE
      
            ; Calculate RootDirSectors
            ; Based on information from https://staff.washington.edu/dittrich/misc/fatgen103.pdf
            ; RootDirSectors = ((PTR_BPB_NUMROOT * 32) + (PTR_BPB_BPLS – 1)) / PTR_BPB_BPLS;

            m_copy16 PTR_BPB_NUMROOT, MR_1                    ; Multiply BPB_NUMROOT by 32
            LDY #$05
@MUL32:     ASL MR_1
            ROL MR_1+1
            DEY
            BNE @MUL32                                        ; After completion, MR_1 contains (PTR_BPB_NUMROOT * 32)
            m_copy16 PTR_BPB_BPLS, MR_2                       ; Copy PTR_BPB_BPLS to MR_2
            m_dec16 MR_2                                      ; And subtract 1
            JSR add                                           ; Add MR_1 to MR_2
            m_copy16 MR_3, MR_2                               ; Copy result to MR_3 (dividend)
            m_copy16 PTR_BPB_BPLS, MR_1                       ; Copy PTR_BPB_BPLS to MR_1 (divisor)
            JSR divide                                        ; And Divide
            m_copy16 MR_2, FAT_ROOTSECTS                      ; MR_2 now contains the size of root sectors, save result

            ;
            ; Calculate FirstDataSector
            ;
            ; FirstDatasector = (Logical sectors per FAT * Number of FATs) + Root Sectors + Reserved sectors
            ;
            m_copy16 PTR_BPB_LOGSECPERFAT, MR_1
            stz MR_2+1
            lda PTR_BPB_NUMFATS
            STA MR_2
            JSR mult16
            m_copy16 MR_3, MR_1
            m_copy16 PTR_BPB_RESERVED, MR_2
            JSR add
            m_copy16 MR_3, MR_1
            m_copy16 FAT_ROOTSECTS, MR_2
            JSR add
            m_copy16 MR_3, FAT_DATASTART

            ;
            ; Calculate Number of Data Sectors
            ;
            m_sub16 PTR_BPB_LOGSECTS, FAT_DATASTART
            m_copy16 MR_3, FAT_DATASECTS

            ;
            ; Calculate number or Data Clusters
            ;
            STZ FAT_LSPC+1
            LDA PTR_BPB_LSPC
            STA FAT_LSPC
            CMP #$01
            BEQ @SKIP
            STZ MR_1+1
            STA MR_1
            m_copy16 FAT_DATASECTS, MR_2
            JSR divide
            m_copy16 MR_2, FAT_DATACLUSTERS
            BRA @DONE
@SKIP:      m_copy16 FAT_DATASECTS, FAT_DATACLUSTERS

@DONE:      ; Calculate Root directory start sector
            m_sub16 FAT_DATASTART, FAT_ROOTSECTS
            m_copy16 MR_3, FAT_ROOTSTART
            
            ; Detect FAT Type
            ; FAT12 if < 4085 Clusters
            ; FAT16 if < 65525 Clusters
            ; FAT32 if >= 65525 Clusters;
            ;
            LDA #$0F
            STA MR_2+1
            LDA #$F5
            STA MR_2
            m_copy16 FAT_DATACLUSTERS, MR_1
            JSR lessthan
            BCS @FAT16
            debugmsg "Detected FAT12"
            LDA #$01
            STA FAT_TYPE
            BRA @DETECTED
@FAT16:     LDA #$FF
            STA MR_2+1
            JSR lessthan
            BCS @FAT32
            debugmsg "Detected FAT16"
            LDA #$02
            STA FAT_TYPE
            BRA @DETECTED
@FAT32:     debugmsg "Detected FAT32"
@DETECTED:  RTS

FAT_GETCLUSTER:
            PHX
            LDA FAT_CLUSTERNUM+1
            STA MR_1+1
            CMP #$0F
            BCS @CHECKLSB
            ORA FAT_CLUSTERNUM
            BEQ @EOF
            LDA FAT_CLUSTERNUM
            STA MR_1
            BRA @GETCLUSTER
@EOF:       LDA #ERR::EOK
            PLX
            SEC
            RTS
@CHECKLSB:  LDA FAT_CLUSTERNUM
            CMP #$F0
            BCS @EOF
@GETCLUSTER:
            SEC
            SBC #$02
            STA MR_1
            LDA MR_1+1
            SBC #$00
            STA MR_1+1
            m_copy16 FAT_LSPC, MR_2
            JSR mult16
            m_add16 MR_3, FAT_DATASTART             ; Calculate cluster position on disk.
            m_copy16 MR_3, NEXTSECTOR               ; Data section start + (Cluster * Sectors per cluster)
@NOWRAP:    LDX FAT_LSPC                            ; Loop (Sectors per cluster) times
            m_copy16 FATSECTORDEST, SECTORBUFPTR    ; pointer will be clobbered by FAT operations, so save it
@LOADLOOP:  PHX
            JSR FDCREADLBA
            PLX
            BCS @END
@OK:        DEX
            BEQ @CLSTRDONE                          ; Done loading this cluster
            LDA NEXTSECTOR
            CLC
            ADC #$01
            STA NEXTSECTOR
            LDA NEXTSECTOR+1
            ADC #$00
            STA NEXTSECTOR+1
            BRA @LOADLOOP
@CLSTRDONE: m_copy16 SECTORBUFPTR, FATSECTORDEST          ; Remember where we left off
            JSR FAT_FOLLOW
            BCS @END
            CLC
@END:       PLX
            RTS



; FAT_FOLLOW
; Get next cluster in chain
;
FAT_FOLLOW:
            LDA FAT_TYPE ; 0x01 = FAT12, 0x02 = FAT16
            LSR A
            BCC @FAT16
            ;
            ; FAT-12 Cluster offset = N + (N /2) 
            ; Copy N to MR_1, MR_2, Divide MR_1 by 2 and add to MR_2
            ;
            m_copy16 FAT_CLUSTERNUM, MR_1
            m_copy16 FAT_CLUSTERNUM, MR_2
            LSR MR_1+1
            ROR MR_1
            JSR add
            ; Fat offset now in MR_3
            m_copy16 MR_3, MR_2
            BRA @DONEOFFSET

@FAT16:     LSR A
            BCC @NOTFAT16
            ;
            ; FAT-16 Cluster offset = N * 2
            ;
            m_copy16 FAT_CLUSTERNUM, MR_2
            ASL MR_2
            ROL MR_2+1
            BRA @DONEOFFSET

@NOTFAT16:  LDA #ERR::EIO
            SEC
            RTS

@DONEOFFSET:
            ;
            ; Fat Sector number = Reserved sectors + (Fat offset / Bytes per sector);
            ;
            m_copy16 FAT_BPLS, MR_1       ; MR_1 = Bytes per logical sector
                                          ; MR_2 = Fat Offset
            JSR divide

            ;
            ; MR_2 now contains the result of (Fat offset / Bytes per sector)
            ; MR_3 contains the remainder (In this case FAT_ENTOFFSET aka offset of the FAT cluster within this Fat Sector) 
            ;
            m_copy16 MR_3, FAT_ENTOFFSET   ; Save FAT_ENTOFFSET
            m_copy16 FAT_RESERVED, MR_1
            JSR add                        ; Reserved sectors + (Fat offset / Bytes per sector)
            m_copy16 MR_3, NEXTSECTOR      ; Set as next sector
            m_add16 FAT_RESERVED, FAT_SIZE
            LDA NEXTSECTOR+1
            CMP MR_3+1
            BCC @FATSECTOK
            BNE @FATERR
            LDA NEXTSECTOR
            CMP MR_3
            BCC @FATSECTOK
            BEQ @FATSECTOK
@FATERR:    SEC
            LDA #ERR::EIO
            RTS
@FATSECTOK: LDA #<FATBUF
            STA SECTORBUFPTR
            STA FATSECTORBUFPTR
            LDA #>FATBUF
            STA SECTORBUFPTR+1
            STA FATSECTORBUFPTR+1
            LDA NEXTSECTOR+1
            CMP FAT_CACHED+1
            BNE @NOTCACHED
            LDA NEXTSECTOR
            CMP FAT_CACHED
            BEQ @CACHED
@NOTCACHED: JSR FDCREADLBA
            BCC @OK
            RTS
@OK:        m_copy16 NEXTSECTOR, FAT_CACHED
            LDA FAT_TYPE            ; FAT-12 sometimes needs half a nibble from the next sector
            LSR A                   ; When the Cluster is at position 511 of the FAT
            BCC @CACHED             ; For any other FAT type, skip this step
            LDA NEXTSECTOR
            CLC
            ADC #$01
            STA NEXTSECTOR
            LDA NEXTSECTOR+1
            ADC #$00
            STA NEXTSECTOR+1
            JSR FDCREADLBA
            BCC @FAT12DONE
            RTS
@FAT12DONE: LDA NEXTSECTOR
            SEC
            SBC #$01
            STA NEXTSECTOR
            LDA NEXTSECTOR+1
            SBC #$00
            STA NEXTSECTOR+1
@CACHED:
            ; FATSECTORBUFPTR is a 16-bit pointer into SECTORBUF
            ; Since sectors are 512 bytes it's easier than using X/Y (I think!)
            ;
            LDA #<FATBUF
            STA FATSECTORBUFPTR
            LDA #>FATBUF
            STA FATSECTORBUFPTR+1
            m_add16 FAT_ENTOFFSET,FATSECTORBUFPTR
            m_copy16 MR_3, FATSECTORBUFPTR
            LDA FAT_TYPE
            LSR A
            BCC @FAT16OFFS
            ;
            ; Fat-12 hackery
            ; Even cluster use FAT_ENTOFFSET + the lower nibble of FAT_ENTOFFSET+1 to form a 12-bit cluster id
            ; E.g 0x12 0x34 would translate to cluster 0x412
            ;
            ; Odd clusters use FAT_ENTOFFSET's high nibble as it's low nibble and FAT_ENTOFFSET+1 as the higher 2 nibbles
            ; E.g 0x12 0x34 would translate to cluster 0x341. we basically just right shift 0x3412
            ;
            LDA FAT_CLUSTERNUM
            AND #$01
            BNE @ODD
            LDA (FATSECTORBUFPTR)
            STA FAT_CLUSTERNUM
            JSR FAT_INCBUF
            LDA (FATSECTORBUFPTR)
            AND #$0F
            STA FAT_CLUSTERNUM+1
            BRA @DONE
@ODD:       LDA (FATSECTORBUFPTR)
            STA FAT_CLUSTERNUM
            JSR FAT_INCBUF
            LDA (FATSECTORBUFPTR)
            STA FAT_CLUSTERNUM+1
            CLC
            LDY #$04
@RSHIFT4:   LSR FAT_CLUSTERNUM+1
            ROR FAT_CLUSTERNUM
            DEY
            BNE @RSHIFT4
            LDA FAT_CLUSTERNUM+1
            AND #$0F
            STA FAT_CLUSTERNUM+1
            BRA @DONE
            ;
            ; Fat-16 Offset
            ;
@FAT16OFFS: LDA (FATSECTORBUFPTR)
            STA FAT_CLUSTERNUM
            INC FATSECTORBUFPTR
            LDA (FATSECTORBUFPTR)
            STA FAT_CLUSTERNUM+1
@DONE:      CLC
            RTS

FAT_INCBUF:
            LDA FATSECTORBUFPTR
            CLC
            ADC #$01
            STA FATSECTORBUFPTR
            LDA FATSECTORBUFPTR+1
            ADC #$00
            STA FATSECTORBUFPTR+1
            RTS

; FAT_FIND
; Find the file specified by FAT_FILENAME
; Sets FAT_CLUSTERNUM to the first cluster of the file
;
FAT_FIND:   m_copy16 FAT_ROOTSTART, NEXTSECTOR
            LDA #<SECTORBUF
            STA FATSECTORBUFPTR
            STA SECTORBUFPTR
            LDA #>SECTORBUF
            STA SECTORBUFPTR+1
            STA FATSECTORBUFPTR+1
            JSR FDCREADLBA
            BCC @OK
            RTS
@OK:
@LOOP_FIND: LDY #$00
            LDA (FATSECTORBUFPTR),Y
            BEQ @NOTFOUND
            CMP #$E5
            BEQ @NEXT
            PHY
            TYA
            CLC
            ADC #$0B                  ;
            TAY                       ;
            LDA (FATSECTORBUFPTR),Y   ; FATSECTORBUFPTR,Y points to attrib for directory entry
            PLY                       ; Point back to beginning of directory entry
            AND #$1E                  ; Hidden/System/Vol Label/Directory?
            BNE @NEXT                 ; Then skip to the next file
@COMPARE:   LDY #10
@LOOP:      LDA FAT_FILENAME,Y
            CMP (FATSECTORBUFPTR),Y
            BNE @NEXT
            DEY
            BPL @LOOP

@FOUND:     LDY #$1A
            LDA (FATSECTORBUFPTR),Y
            STA FAT_CLUSTERNUM
            INY
            LDA (FATSECTORBUFPTR),Y
            STA FAT_CLUSTERNUM+1
            CLC
            RTS
@NOTFOUND:  LDA #ERR::ENOENT
            SEC
            RTS

@NEXT:      LDA FATSECTORBUFPTR
            CLC
            ADC #32
            STA FATSECTORBUFPTR
            BCS @CARRY
            BRA @LOOP_FIND
@CARRY:     BBS0 FATSECTORBUFPTR+1,@NEXTSECTOR
            INC FATSECTORBUFPTR+1
            BRA @LOOP_FIND
@NEXTSECTOR:
            m_inc16 NEXTSECTOR
            LDA NEXTSECTOR+1
            CMP FAT_DATASTART+1
            BEQ @HIOK
            BCS @NOTFOUND
@HIOK:      LDA NEXTSECTOR
            CMP FAT_DATASTART
            BCS @NOTFOUND
            LDA #<SECTORBUF
            STA SECTORBUFPTR
            STA FATSECTORBUFPTR
            LDA #>SECTORBUF
            STA SECTORBUFPTR+1
            STA FATSECTORBUFPTR+1
            JSR FDCREADLBA
            BCC @LOOP_FIND
            RTS
