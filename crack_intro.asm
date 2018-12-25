;
; CRACK INTRO
;

; Coding and graphics by T.M.R/Cosine
; Music by Odie/Cosine


; Select an output filename
		!to "crack_intro.prg",cbm


; Yank in binary data
		* = $0820
colour_data	!binary "data/paving.col"

		* = $0900
music		!binary "data/star_trooper.prg",,2

		* = $2a00
screen_data	!binary "data/paving.map"

		* = $2e00
		!binary "data/copyright.spr"

		* = $3800
		!binary "data/paving.chr"

		* = $3e40
		!binary "data/cracks.spr"


; Constants
rstr1p		= $00
rstr2p		= $81
rstr3p		= $e9

; Labels
rn		= $50
irq_store	= $51

border_col_cnt	= $52
screen_col_cnt	= $53

cursor_state	= $54
cursor_tmr	= $55

cos_at_1	= $56
cos_offset_1	= $f4		; constant

cos_at_2	= $57
cos_speed_2	= $03		; constant

cos_at_3	= $58
cos_speed_3	= $02		; constant

cos_at_4	= $59
cos_speed_4	= $03		; constant

sprite_x_offset	= $5a

scroll_x	= $5b
scroll_speed	= $5c

line_buffer	= $03d0		; $29 bytes used
char_buffer	= $03ff

font_work	= $3000


; Entry point at $1300
		* = $1740
entry		sei

; Bank out the ROMs and set up NMI and raster IRQ interrupts
		lda #$35
		sta $01

		lda #<nmi
		sta $fffa
		lda #>nmi
		sta $fffb

		lda #<int
		sta $fffe
		lda #>int
		sta $ffff

		lda #$7f
		sta $dc0d
		sta $dd0d

		lda $dc0d
		lda $dd0d

		lda #rstr1p
		sta $d012

		lda #$0b
		sta $d011

		lda #$01
		sta $d019
		sta $d01a

; Character set build loop (one shot, self-modifying code)
		lda #$32
		sta $01

		ldx #$00
font_generate	stx cos_at_1

		lda font_encode,x
		sta font_read+$01
		lda #$00

		asl font_read+$01
		rol
		asl font_read+$01
		rol
		asl font_read+$01
		rol

		clc
		adc #$d0
		sta font_read+$02

; Copy character to buffer for building
		ldx #$00
font_read	lda $d000
		sta line_buffer,x
		inc font_read+$01
		bne *+$05
		inc font_read+$02
		inx
		cpx #$08
		bne font_read

; Copy current buffer to target font
		ldy #$00
char_build	ldx #$00
cb_loop		lda line_buffer,x
font_write	sta font_work
		inc font_write+$01
		bne *+$05
		inc font_write+$02
		inx
		cpx #$08
		bne cb_loop

		ldx #$00
char_shunt	lda line_buffer,x
		asl
		adc #$00
		sta line_buffer,x
		inx
		cpx #$08
		bne char_shunt

		iny
		cpy #$08
		bne char_build

		ldx cos_at_1
		inx
		cpx #$20
		bne font_generate

		lda #$35
		sta $01

; Set up the screen
		ldx #$00
screen_init_1	lda screen_data+$000,x
		sta $0400,x
		tay
		lda colour_data,y
		sta $d800,x

		lda screen_data+$100,x
		sta $0500,x
		tay
		lda colour_data,y
		sta $d900,x

		lda screen_data+$200,x
		sta $0600,x
		tay
		lda colour_data,y
		sta $da00,x

		lda screen_data+$2e8,x
		sta $06e8,x
		tay
		lda colour_data,y
		sta $dae8,x

		inx
		bne screen_init_1

; Set the scrolling message's colour
		ldx #$00
		lda #$0e
screen_init_2	sta $db98,x
		inx
		cpx #$28
		bne screen_init_2

; Clear the label space and set some specific values
		ldx #$20
		lda #$00
nuke_zp		sta $00,x
		inx
		bne nuke_zp

		lda #$01
		sta rn

		lda #$80
		sta sprite_x_offset

; Reset the scrolling message and wipe the line buffer
		jsr reset

		lda #$01
		sta scroll_speed

		ldx #$00
		txa
line_buff_clr	sta line_buffer,x
		inx
		cpx #$29
		bne line_buff_clr

; Initialise the music
		lda #$00
		jsr music+$00

		cli


; Check to see if space has been pressed
main_loop	lda $dc01
		cmp #$ef
		beq *+$05
		jmp main_loop

; Reset some registers
		sei

		lda #$00
		sta $d011
		sta $d020
		sta $d021
		sta $d418

; Reset the C64 (a linker would go here...)
		lda #$37
		sta $01

		jmp $fce2


; IRQ interrupt
int		pha
		txa
		pha
		tya
		pha

		lda $d019
		and #$01
		sta $d019
		bne ya
		jmp ea31

ya		lda rn
		cmp #$02
		bne *+$05
		jmp rout2

		cmp #$03
		bne *+$05
		jmp rout3


; Raster split 1
rout1		ldx border_col_cnt
		lda border_cols,x
		sta $d020
		lda border_d011s,x
		sta $d011
		inx
		cpx #$16
		bne *+$04
		ldx #$15
		stx border_col_cnt

		ldx screen_col_cnt
		lda border_cols,x
		sta $d021
		inx
		cpx #$26
		bne *+$04
		ldx #$25
		stx screen_col_cnt

		lda #$08
		sta $d016
		lda #$1e
		sta $d018

; First positions for the sprites
		lda #$ff
		sta $d015

		ldx #$00
sprite_init_1a	lda sprite_pos_1,x
		sta $d000,x
		inx
		cpx #$11
		bne sprite_init_1a

		ldx #$00
sprite_init_1b	lda sprite_cols_1,x
		sta $d027,x
		lda sprite_dps_1,x
		sta $07f8,x
		inx
		cpx #$08
		bne sprite_init_1b

; Update the cursor animation
		ldx cursor_tmr
		inx
		cpx #$10
		bne cursor_noflash

		lda cursor_state
		eor #$01
		sta cursor_state

		ldx #$00
cursor_noflash	stx cursor_tmr

; Refresh the cursor
		ldx cursor_state
		lda cursor_data+$00,x
		sta $04f0
		lda cursor_data+$02,x
		sta $0518

; Process the scroller
		ldy scroll_speed

scroll_update	sty irq_store

		ldx scroll_x
		inx
		cpx #$08
		bne scr_xb

		ldx #$00
mover		lda line_buffer+$01,x
		sta line_buffer+$00,x
		inx
		cpx #$28
		bne mover

mread		ldy scroll_text
		bne okay
		jsr reset
		jmp mread

okay		cpy #$40
		bcc okay_2

		tya
		and #$0f
		sta scroll_speed

		ldy #$20

okay_2		lda text_decode,y
		asl
		asl
		asl
		sta line_buffer+$28

		inc mread+$01
		bne *+$05
		inc mread+$02

		lda cos_at_1
		clc
		adc #cos_offset_1
		sta cos_at_1

		ldx #$00
scr_xb		stx scroll_x

		ldy irq_store
		dey
		bne scroll_update

; Render the scroller's buffer to the screen
		ldy cos_at_1
		iny
		sty cos_at_1

		lda scroll_x_cos,y
		and #$07
		clc
		adc line_buffer+$00
		sta char_buffer

		tya
		clc
		adc #cos_offset_1
		tay

		ldx #$00
line_draw	lda scroll_x_cos,y
		and #$07
		clc
		adc line_buffer+$01,x
		sta $0798,x
		tya
		clc
		adc #cos_offset_1
		tay
		inx
		cpx #$28
		bne line_draw

; Render the scroller's sprite-based character
		lda char_buffer
		sta def_copy+$01
		lda #$00
		asl def_copy+$01
		rol
		asl def_copy+$01
		rol
		asl def_copy+$01
		rol
		clc
		adc #>font_work
		sta def_copy+$02

; Character to sprite copy loop
		ldx #$00
		ldy #$00
def_copy	lda font_work,x
		sta $3f08,y
		iny
		iny
		iny
		inx
		cpx #$08
		bne def_copy

; Set the scroller sprite's X position
		lda scroll_x
		eor #$07
		sta sprite_pos_2+$0a

; Set up for the second interrupt (uses the logo Y position)
		lda #$02
		sta rn
		lda sprite_pos_1+$07
		clc
		adc #$0c
		sta $d012

; Exit this interrupt
		jmp ea31


; Raster split 2
rout2

; Relocate the sprites
		ldx #$00
sprite_init_2a	lda sprite_pos_2,x
		sta $d000,x
		inx
		cpx #$11
		bne sprite_init_2a

		ldx #$00
sprite_init_2b	lda sprite_cols_2,x
		sta $d027,x
		lda sprite_dps_2,x
		sta $07f8,x
		inx
		cpx #$08
		bne sprite_init_2b

; Set up for the third interrupt
		lda #$03
		sta rn
		lda #rstr3p
		sta $d012

; Exit this interrupt
		jmp ea31


; Raster split 3
rout3		ldx #$02
		dex
		bne *-$01
		bit $ea

		ldy #$1c
		lda scroll_x
		and #$07
		eor #$07
		ora #$08

		sta $d016
		sty $d018

; Update the copyright symbol's X position
		lda cos_at_2
		clc
		adc #cos_speed_2
		sta cos_at_2
		tax

		lda cos_at_3
		clc
		adc #cos_speed_3
		cmp #$fa
		bcc *+$04
		lda #$07
		sta cos_at_3
		tay

		lda #$00
		sta sprite_msb_1
		sta sprite_msb_2

; First column of copyright sprites
		lda sprite_x_offset
		clc
		adc sprite_x_cos,x
		bcc no_msb_1a

		sta irq_store
		lda sprite_msb_1
		ora #%00001001
		sta sprite_msb_1
		lda irq_store

no_msb_1a	clc
		adc sprite_x_cos,y
		bcc no_msb_1b

		sta irq_store
		lda sprite_msb_1
		ora #%00001001
		sta sprite_msb_1
		lda irq_store

no_msb_1b	sta sprite_pos_1+$00
		sta sprite_pos_1+$06
		sta sprite_pos_2+$00
		sta sprite_pos_2+$06

; Second column of copyright sprites
		lda sprite_x_offset
		clc
		adc #$18
		clc
		adc sprite_x_cos,x
		bcc no_msb_2a

		sta irq_store
		lda sprite_msb_1
		ora #%00000010
		sta sprite_msb_1
		lda irq_store

no_msb_2a	clc
		adc sprite_x_cos,y
		bcc no_msb_2b

		sta irq_store
		lda sprite_msb_1
		ora #%00000010
		sta sprite_msb_1
		lda irq_store

no_msb_2b	sta sprite_pos_1+$02
		sta sprite_pos_2+$02

; Third column of copyright sprites
		lda sprite_x_offset
		clc
		adc #$30
		clc
		adc sprite_x_cos,x
		bcc no_msb_3a

		sta irq_store
		lda sprite_msb_1
		ora #%00010100
		sta sprite_msb_1
		lda irq_store

no_msb_3a	clc
		adc sprite_x_cos,y
		bcc no_msb_3b

		sta irq_store
		lda sprite_msb_1
		ora #%00010100
		sta sprite_msb_1
		lda irq_store

no_msb_3b	sta sprite_pos_1+$04
		sta sprite_pos_1+$08
		sta sprite_pos_2+$04
		sta sprite_pos_2+$08

		lda sprite_msb_1
		sta sprite_msb_2

; Decrease X offset to make the copyright move on from the right
		ldx sprite_x_offset
		dex
		cpx #$18
		bcc *+$04
		stx sprite_x_offset

; Update the copyright symbol's Y position
		lda cos_at_4
		clc
		adc #cos_speed_4
		sta cos_at_4

		tax
		lda sprite_y_cos,x
		sta sprite_pos_1+$01
		sta sprite_pos_1+$03
		sta sprite_pos_1+$05
		clc
		adc #$15
		sta sprite_pos_1+$07
		sta sprite_pos_2+$07
		sta sprite_pos_1+$09
		sta sprite_pos_2+$09
		clc
		adc #$15
		sta sprite_pos_2+$01
		sta sprite_pos_2+$03
		sta sprite_pos_2+$05

; Play the music
		jsr music+$03

; Set up for the first interrupt
		lda #$01
		sta rn
		lda #rstr1p
		sta $d012

; Exit this interrupt
ea31		pla
		tay
		pla
		tax
		pla
nmi		rti


; Reset code for the scroller's self mod
reset		lda #<scroll_text
		sta mread+$01
		lda #>scroll_text
		sta mread+$02

		rts


; Screen and border colour strobe tables
border_cols	!byte $01,$01,$01,$0d,$01,$0d,$0d,$0d
		!byte $03,$0d,$03,$03,$03,$05,$03,$05
		!byte $05,$05,$0e,$05,$0e,$0e,$0e,$04
		!byte $0e,$04,$04,$04,$0b,$04,$0b,$0b
		!byte $0b,$06,$0b,$06,$06,$06,$06,$06

border_d011s	!byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b
		!byte $1b,$0b,$1b,$1b,$1b,$1b,$1b,$1b
		!byte $1b,$1b,$1b,$1b,$1b,$1b,$1b,$1b
		!byte $1b,$1b,$1b,$1b,$1b,$1b,$1b,$1b

; Upper sprite positions, colours and data pointers
sprite_pos_1	!byte $30,$00,$48,$00,$60,$00,$30,$00
		!byte $60,$00,$3f,$38,$b0,$3a,$b0,$4a
sprite_msb_1	!byte $00

sprite_cols_1	!byte $0f,$0f,$0f,$0f,$0f,$0e,$0e,$0e

sprite_dps_1	!byte $b8,$b9,$ba,$bb,$bc,$f9,$fa,$fb

; Lower sprite positions, colours and data pointers
sprite_pos_2	!byte $30,$00,$48,$00,$60,$00,$30,$00
		!byte $60,$00,$00,$e8,$28,$e8,$c2,$e8
sprite_msb_2	!byte $00

sprite_cols_2	!byte $0f,$0f,$0f,$0f,$0f,$0e,$00,$00

sprite_dps_2	!byte $bd,$be,$bf,$bb,$bc,$fc,$fd,$fe


; Sprite cosine tables
sprite_x_cos	!byte $7d,$7d,$7d,$7d,$7d,$7d,$7d,$7d
		!byte $7c,$7c,$7c,$7b,$7b,$7a,$7a,$79
		!byte $79,$78,$77,$77,$76,$75,$75,$74
		!byte $73,$72,$71,$70,$6f,$6e,$6d,$6c
		!byte $6b,$6a,$69,$68,$66,$65,$64,$63
		!byte $61,$60,$5f,$5d,$5c,$5b,$59,$58
		!byte $57,$55,$54,$52,$51,$4f,$4e,$4c
		!byte $4b,$49,$48,$46,$45,$43,$41,$40

		!byte $3e,$3d,$3b,$3a,$38,$37,$35,$34
		!byte $32,$31,$2f,$2e,$2c,$2b,$29,$28
		!byte $26,$25,$23,$22,$21,$1f,$1e,$1d
		!byte $1b,$1a,$19,$18,$16,$15,$14,$13
		!byte $12,$11,$10,$0f,$0e,$0d,$0c,$0b
		!byte $0a,$09,$08,$08,$07,$06,$05,$05
		!byte $04,$04,$03,$03,$02,$02,$01,$01
		!byte $01,$00,$00,$00,$00,$00,$00,$00

		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $01,$01,$01,$02,$02,$03,$03,$04
		!byte $04,$05,$06,$06,$07,$08,$09,$09
		!byte $0a,$0b,$0c,$0d,$0e,$0f,$10,$11
		!byte $12,$13,$14,$16,$17,$18,$19,$1a
		!byte $1c,$1d,$1e,$20,$21,$22,$24,$25
		!byte $27,$28,$2a,$2b,$2d,$2e,$30,$31
		!byte $33,$34,$36,$37,$39,$3a,$3c,$3d

		!byte $3f,$40,$42,$43,$45,$47,$48,$4a
		!byte $4b,$4d,$4e,$50,$51,$53,$54,$56
		!byte $57,$58,$5a,$5b,$5d,$5e,$5f,$61
		!byte $62,$63,$64,$66,$67,$68,$69,$6a
		!byte $6b,$6c,$6d,$6e,$6f,$70,$71,$72
		!byte $73,$74,$75,$76,$76,$77,$78,$78
		!byte $79,$79,$7a,$7a,$7b,$7b,$7c,$7c
		!byte $7c,$7d,$7d,$7d,$7d,$7d,$7d,$7d

sprite_y_cos	!byte $b9,$b9,$b9,$b9,$b9,$b9,$b9,$b9
		!byte $b8,$b8,$b7,$b7,$b7,$b6,$b6,$b5
		!byte $b4,$b4,$b3,$b2,$b2,$b1,$b0,$af
		!byte $ae,$ad,$ac,$ab,$aa,$a9,$a8,$a7
		!byte $a6,$a5,$a3,$a2,$a1,$a0,$9e,$9d
		!byte $9c,$9a,$99,$97,$96,$95,$93,$92
		!byte $90,$8f,$8d,$8b,$8a,$88,$87,$85
		!byte $83,$82,$80,$7f,$7d,$7b,$7a,$78

		!byte $76,$75,$73,$71,$70,$6e,$6d,$6b
		!byte $69,$68,$66,$64,$63,$61,$60,$5e
		!byte $5d,$5b,$5a,$58,$57,$55,$54,$53
		!byte $51,$50,$4e,$4d,$4c,$4b,$49,$48
		!byte $47,$46,$45,$44,$43,$42,$41,$40
		!byte $3f,$3e,$3d,$3c,$3b,$3b,$3a,$39
		!byte $39,$38,$37,$37,$36,$36,$35,$35
		!byte $35,$34,$34,$34,$34,$34,$34,$34

		!byte $34,$34,$34,$34,$34,$34,$34,$35
		!byte $35,$35,$36,$36,$36,$37,$38,$38
		!byte $39,$39,$3a,$3b,$3c,$3c,$3d,$3e
		!byte $3f,$40,$41,$42,$43,$44,$45,$46
		!byte $47,$49,$4a,$4b,$4c,$4e,$4f,$50
		!byte $52,$53,$54,$56,$57,$59,$5a,$5c
		!byte $5d,$5f,$60,$62,$63,$65,$67,$68
		!byte $6a,$6b,$6d,$6f,$70,$72,$74,$75

		!byte $77,$79,$7a,$7c,$7d,$7f,$81,$82
		!byte $84,$86,$87,$89,$8a,$8c,$8d,$8f
		!byte $90,$92,$93,$95,$96,$98,$99,$9b
		!byte $9c,$9d,$9f,$a0,$a1,$a3,$a4,$a5
		!byte $a6,$a7,$a8,$aa,$ab,$ac,$ad,$ae
		!byte $ae,$af,$b0,$b1,$b2,$b3,$b3,$b4
		!byte $b5,$b5,$b6,$b6,$b7,$b7,$b8,$b8
		!byte $b8,$b9,$b9,$b9,$b9,$b9,$b9,$b9

; Scroller cosine table
scroll_x_cos	!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$1f,$1f,$1f,$1f,$1f
		!byte $1e,$1e,$1e,$1d,$1d,$1d,$1c,$1c
		!byte $1c,$1b,$1b,$1a,$1a,$1a,$19,$19
		!byte $18,$18,$17,$17,$16,$16,$15,$15
		!byte $14,$14,$13,$13,$12,$12,$11,$10
		!byte $10,$0f,$0f,$0e,$0e,$0d,$0d,$0c
		!byte $0c,$0b,$0b,$0a,$0a,$09,$09,$08

		!byte $08,$07,$07,$06,$06,$05,$05,$05
		!byte $04,$04,$04,$03,$03,$03,$02,$02
		!byte $02,$01,$01,$01,$01,$01,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$01,$01,$01,$01,$02
		!byte $02,$02,$02,$03,$03,$03,$04,$04
		!byte $04,$05,$05,$06,$06,$07,$07,$07

		!byte $08,$08,$09,$09,$0a,$0a,$0b,$0b
		!byte $0c,$0c,$0d,$0d,$0e,$0f,$0f,$10
		!byte $10,$11,$11,$12,$12,$13,$13,$14
		!byte $14,$15,$15,$16,$17,$17,$17,$18
		!byte $18,$19,$19,$1a,$1a,$1b,$1b,$1b
		!byte $1c,$1c,$1d,$1d,$1d,$1e,$1e,$1e
		!byte $1e,$1f,$1f,$1f,$1f,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20

		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20
		!byte $20,$20,$20,$20,$20,$20,$20,$20

; Font encode table (says which characters to convert during start up)
font_encode	!byte $20,$01,$02,$03,$04,$05,$06,$07	; @ to G
		!byte $08,$09,$0a,$0b,$0c,$0d,$0e,$0f	; H to O
		!byte $10,$11,$12,$13,$14,$15,$16,$17	; P to W
		!byte $18,$19,$1a,$21,$2e,$2c,$2a,$3f	; X to Z and punctuation

; Font decode table (remaps punctuation to the characters selected above)
text_decode	!byte $00,$01,$02,$03,$04,$05,$06,$07	; @ to G
		!byte $08,$09,$0a,$0b,$0c,$0d,$0e,$0f	; H to O
		!byte $10,$11,$12,$13,$14,$15,$16,$17	; P to W
		!byte $18,$19,$1a,$00,$00,$00,$00,$00	; X to Z
		!byte $00,$1b,$00,$00,$00,$00,$00,$00	; space to '
		!byte $00,$00,$1e,$00,$1d,$00,$1c,$00	; ( to /
		!byte $00,$00,$00,$00,$00,$00,$00,$00	; 0 to 7
		!byte $00,$00,$00,$00,$00,$00,$00,$1f	; 8 to ?

; Scrolling message text
scroll_text	!scr $41,"**** crack intro ****"
		!scr "        "

		!scr $42,"coding and graphics this time from "
		!scr "t.m.r, accompanied by odie on the sid chip"
		!scr "      "

		!scr $43,"this fractured scroller is fairly hard "
		!scr "to read and, as an added bonus, there are "
		!scr "no numbers or much in the way of "
		!scr "punctuation available since each character "
		!scr "needs eight preshifted states which must "
		!scr "all be stored within the one font!"
		!scr "   "

		!scr $44,"going back to writing text without "
		!scr "apostrophes, hyphens or numbers proved "
		!scr "surprisingly difficult after years of "
		!scr "relying on them, but this scroller does "
		!scr "run on a forty column wide screen so that "
		!scr "counts for something...  right?"
		!scr "      "

		!scr $43,"i have very little to say today apart "
		!scr "from the traditional, dark mutterings "
		!scr "about the season, but i doubt anybody "
		!scr "will actually be reading unless they are "
		!scr "picking through the memory or looking at "
		!scr "my source code on github, so there is no "
		!scr "great need for me to write anything "
		!scr "sensible or fill the available ram!"
		!scr "      "

		!scr $42,"let us get the greetings out of the "
		!scr "way before this gets bundled up for "
		!scr "release..."
		!scr "      "

		!scr $44,"hello from the happy bunnies in cosine "
		!scr "to"
		!scr "   "

		!scr $45,"absence * "
		!scr "abyss connection * "
		!scr "arkanix labs * "
		!scr "artstate * "
		!scr "ate bit * "
		!scr "atlantis * "

		!scr "booze design * "

		!scr "camelot * "
		!scr "censor design * "
		!scr "chorus * "
		!scr "chrome * "
		!scr "cncd * "
		!scr "cpu * "
		!scr "crescent * "
		!scr "crest * "
		!scr "covert bitops * "

		!scr "defence force * "
		!scr "dekadence * "
		!scr "desire * "
		!scr "dac * "
		!scr "dmagic * "
		!scr "dual crew * "

		!scr "exclusive on * "

		!scr "fairlight * "
		!scr "f4cg * "
		!scr "fire * "
		!scr "flat 3 * "
		!scr "focus * "
		!scr "french touch * "
		!scr "funkscientist productions * "

		!scr "genesis project * "
		!scr "gheymaid inc * "

		!scr "hitmen * "
		!scr "hoaxers * "
		!scr "hokuto force * "

		!scr "legion of doom * "
		!scr "level sixty four * "

		!scr "maniacs of noise * "
		!scr "mayday * "
		!scr "meanteam * "
		!scr "metalvotze * "

		!scr "noname * "
		!scr "nostalgia * "
		!scr "nuance * "

		!scr "offence * "
		!scr "onslaught * "
		!scr "orb * "
		!scr "oxyron * "

		!scr "padua * "
		!scr "performers * "
		!scr "plush * "
		!scr "ppcs * "
		!scr "psytronik * "

		!scr "reptilia * "
		!scr "resource * "
		!scr "rgcd * "

		!scr "secure * "
		!scr "shape * "
		!scr "side b * "
		!scr "singular * "
		!scr "slash * "
		!scr "slipstream * "
		!scr "success and trc * "
		!scr "style * "
		!scr "suicyco industries * "

		!scr "taquart * "
		!scr "tempest * "
		!scr "tek * "
		!scr "triad * "
		!scr "tristar and red sector * "

		!scr "viruz * "
		!scr "vision * "

		!scr "wow * "
		!scr "wrath designs * "
		!scr "xenon * "
		!scr "xentax"
		!scr "      "

		!scr $42,"go visit the cosine website at "
		!scr $41,"**** cosine.org.uk ****"

		!scr $42,"or my blog type thingy behind "
		!scr $41,"**** jasonkelk.me.uk ****"

		!scr $43,"and we are done for another scrolling "
		!scr "message, so bah humbug from t.m.r and see "
		!scr "you all next time... .. .  .   ."
		!scr "                "

		!byte $00

; Cursor flash animation data
cursor_data	!byte $63,$00
		!byte $66,$00