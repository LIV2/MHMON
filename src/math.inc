
MR_1 = $0600
MR_2 = MR_1 + 4
MR_3 = MR_2 + 4
MR_4 = MR_3 + 4

lessthan:	lda MR_1+1
			cmp MR_2+1
			bcs @end
			lda MR_1
			cmp MR_2
@end:		rts

.macro		m_copy16 addra, addrb
			lda addra
			sta addrb
			lda addra+1
			sta addrb+1
.endmacro

.macro 		m_dec16 addr
			lda addr
			bne @nowrap
			dec addr+1
@nowrap:	dec addr
.endmacro

.macro		m_inc16 addr
			inc addr
			bne @nowrap
			inc addr+1
@nowrap:
.endmacro


;16-Bit addition
.macro		m_add16 adda, addb
			LDA adda
			STA MR_1
			LDA adda+1
			STA MR_1+1
			LDA addb
			sta MR_2
			LDA addb+1
			STA MR_2+1
			JSR add
.endmacro

add:		STZ MR_3+2
			STZ MR_3+3
			CLC
			LDA MR_1
			ADC MR_2
			STA MR_3
			LDA MR_1+1
			ADC MR_2+1
			STA MR_3+1
			LDA #$00
			ADC #$00
			STA MR_3+2
			RTS

.macro		m_sub16 suba, subb
			m_copy16 suba, MR_1
			m_copy16 subb, MR_2
			JSR sub
.endmacro

sub:		sec
			lda MR_1
			sbc MR_2
			sta MR_3
			lda MR_1+1
			sbc MR_2+1
			sta MR_3+1
			rts

;16-bit multiply with 32-bit product
;took from 6502.org

.macro 		m_mult16 multiplier, multiplicand
			lda multiplier
			sta MR_1
			lda multiplier+1
			sta MR_1+1
			lda multiplicand
			sta MR_2
			lda multiplicand+1
			sta MR_2+1
			jsr mult16
.endmacro

multiplier		= MR_1
multiplicand	= MR_2
product			= MR_3
 
mult16:		lda	#$00
			sta	MR_3+2			; clear upper bits of product
			sta	MR_3+3 
			ldx	#$10			; set binary count to 16
shift_r:	lsr	MR_1+1			; divide multiplier by 2
			ror	MR_1
			bcc	rotate_r
			lda	MR_3+2			; get upper half of product and add multiplicand
			clc
			adc	MR_2
			sta	MR_3+2
			lda	MR_3+3
			adc	MR_2+1
rotate_r:	ror					; rotate partial product
			sta	MR_3+3
			ror	MR_3+2
			ror	MR_3+1
			ror	MR_3
			dex
			bne	shift_r
			rts

; 16-bit divide with 16-bit result
; Source: http://codebase64.org/doku.php?id=base:16bit_division_16-bit_result

.macro 			m_div16 dividend,divisor
				lda divisor
				sta MR_1
				lda divisor+1
				sta MR_1+1
				lda dividend
				sta MR_2
				lda dividend+1
				sta MR_2+1
				jsr divide
				LDA MR_2
				STA MR_3
				LDA MR_2+1
				STA MR_3+1
				LDA MR_2+2
				STA MR_3+2
.endmacro

;divisor		= MR_1		;$59 used for hi-byte
;dividend		= MR_2		;$fc used for hi-byte
;remainder		= MR_3		;$fe used for hi-byte
result			= MR_2		;save memory by reusing divident to store the result

divide:			lda #0	        ;preset remainder to 0
				sta MR_3
				sta MR_3+1
				ldx #16	        	;repeat for each bit: ...

divloop:		asl MR_2	;dividend lb & hb*2, msb -> Carry
				rol MR_2+1	
				rol MR_3		;remainder lb & hb * 2 + msb from carry
				rol MR_3+1
				lda MR_3
				sec
				sbc MR_1			;substract divisor to see if it fits in
				tay	        		;lb result -> Y, for we may need it later
				lda MR_3+1
				sbc MR_1+1
				bcc skip			;if carry=0 then divisor didn't fit in yet

				sta MR_3+1		;else save substraction result as new remainder,
				sty MR_3	
				inc result			;and INCrement result cause divisor fit in 1 times

skip:			dex
				bne divloop
				rts