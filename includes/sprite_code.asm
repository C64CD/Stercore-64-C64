; Clear all of the sprites from the playfield
sprite_clear

; Clear the static starfield
		ldy #$00
		sty bitmap_ram+$0668+(star_1_x*8)
		sty bitmap_ram+$08e8+(star_2_x*8)
		sty bitmap_ram+$0b68+(star_3_x*8)
		sty bitmap_ram+$0de8+(star_4_x*8)
		sty bitmap_ram+$1068+(star_5_x*8)
		sty bitmap_ram+$12e8+(star_6_x*8)
		sty bitmap_ram+$1568+(star_7_x*8)
		sty bitmap_ram+$17e8+(star_8_x*8)
		sty bitmap_ram+$1a68+(star_9_x*8)
		sty bitmap_ram+$1ce8+(star_10_x*8)

; Clear the player sprite
		lda player_x
		and #$fc
		asl
		sta sprite_x_off
		lda player_y
		asl
		tay

		jsr clear_sprite

; Clear the player bullet sprite
		lda bullet_x
		and #$fc
		asl
		sta sprite_x_off
		lda bullet_y
		asl
		tay

; Update scanline $00
		lda screen_offset_l,y
		sta sprite_write+$00
		lda screen_offset_h,y
		sta sprite_write+$01
		sty sprite_count

		ldy sprite_x_off
		lda #$00
		sta (sprite_write),y

; Update scanline $09
		lda sprite_count
		clc
		adc #$09
		tay

		lda screen_offset_l,y
		sta sprite_write+$00
		lda screen_offset_h,y
		sta sprite_write+$01

		ldy sprite_x_off
		lda #$00
		sta (sprite_write),y

; Clear enemy 1 sprite
		lda enemy_1_x
		and #$fc
		asl
		sta sprite_x_off
		lda enemy_1_y
		asl
		tay

		jsr clear_sprite

; Clear enemy 2 sprite
		lda enemy_2_x
		and #$fc
		asl
		sta sprite_x_off
		lda enemy_2_y
		asl
		tay

		jsr clear_sprite

; Clear enemy 3 sprite
		lda enemy_3_x
		and #$fc
		asl
		sta sprite_x_off
		lda enemy_3_y
		asl
		tay

		jsr clear_sprite

; Clear enemy 4 sprite
		lda enemy_4_x
		and #$fc
		asl
		sta sprite_x_off
		lda enemy_4_y
		asl
		tay

		jsr clear_sprite

		rts


; Draw all of the sprites into the playfield
sprite_draw

; Draw the static starfield (skip of the attribute cell isn't $f0
		ldy #$08
		lda attrib_ram+$0ce+star_1_x	;needs to be one to the right
		cmp #$f0
		bne *+$05
		sty bitmap_ram+$0668+(star_1_x*8)

		lda attrib_ram+$11e+star_2_x	;needs to be one to the right
		cmp #$f0
		bne *+$05
		sty bitmap_ram+$08e8+(star_2_x*8)

		lda attrib_ram+$16e+star_3_x	;needs to be one to the right
		cmp #$f0
		bne *+$05
		sty bitmap_ram+$0b68+(star_3_x*8)

		lda attrib_ram+$1be+star_4_x	;needs to be one to the right
		cmp #$f0
		bne *+$05
		sty bitmap_ram+$0de8+(star_4_x*8)

		lda attrib_ram+$20e+star_5_x	;needs to be one to the right
		cmp #$f0
		bne *+$05
		sty bitmap_ram+$1068+(star_5_x*8)

		lda attrib_ram+$25e+star_6_x	;needs to be one to the right
		cmp #$f0
		bne *+$05
		sty bitmap_ram+$12e8+(star_6_x*8)

		lda attrib_ram+$2ae+star_7_x	;needs to be one to the right
		cmp #$f0
		bne *+$05
		sty bitmap_ram+$1568+(star_7_x*8)

		lda attrib_ram+$2fe+star_8_x	;needs to be one to the right
		cmp #$f0
		bne *+$05
		sty bitmap_ram+$17e8+(star_8_x*8)

		lda attrib_ram+$34e+star_9_x	;needs to be one to the right
		cmp #$f0
		bne *+$05
		sty bitmap_ram+$1a68+(star_9_x*8)

		lda attrib_ram+$39e+star_10_x	;needs to be one to the right
		cmp #$f0
		bne *+$05
		sty bitmap_ram+$1ce8+(star_10_x*8)

; Draw the player sprite
; (Skip this if the lowest bit of player_shield is set to make the ship flash)
		lda player_shield
		lsr
		bcs shield_skip

		lda player_x
		and #$fc
		asl
		sta sprite_x_off
		lda player_x
		and #$03
		tax
		lda player_y
		asl
		tay

		jsr draw_sprite

; Draw the player bullet sprite
shield_skip	lda bullet_x
		and #$fc
		asl
		sta sprite_x_off
		lda bullet_y
		asl
		tay

; Update scanline $00
		lda screen_offset_l,y
		sta sprite_write+$00
		lda screen_offset_h,y
		sta sprite_write+$01
		sty sprite_count

		ldy sprite_x_off
		lda #$97
		sta (sprite_write),y

; Update scanline $09
		lda sprite_count
		clc
		adc #$09
		tay

		lda screen_offset_l,y
		sta sprite_write+$00
		lda screen_offset_h,y
		sta sprite_write+$01

		ldy sprite_x_off
		lda #$97
		sta (sprite_write),y

; Draw enemy 1 sprite
		lda enemy_1_x
		and #$fc
		asl
		sta sprite_x_off
		lda enemy_1_x
		and #$03
		clc
		adc #$08
		tax
		lda enemy_1_y
		asl
		tay

		jsr draw_sprite

; Draw enemy 2 sprite
		lda enemy_2_x
		and #$fc
		asl
		sta sprite_x_off
		lda enemy_2_x
		and #$03
		clc
		adc #$08
		tax
		lda enemy_2_y
		asl
		tay

		jsr draw_sprite

; Draw enemy 3 sprite
		lda enemy_3_x
		and #$fc
		asl
		sta sprite_x_off
		lda enemy_3_x
		and #$03
		clc
		adc #$04
		tax
		lda enemy_3_y
		asl
		tay

		jsr draw_sprite

; Draw enemy 4 sprite
		lda enemy_4_x
		and #$fc
		asl
		sta sprite_x_off
		lda enemy_4_x
		and #$03
		clc
		adc #$04
		tax
		lda enemy_4_y
		asl
		tay

		jsr draw_sprite

		rts


; Sprite rendering subroutine
; X selects which sprite definition to use, Y is height
draw_sprite	lda multi_0a,x
		tax

; Update scanline $00
		lda screen_offset_l,y
		sta sprite_write+$00
		lda screen_offset_h,y
		sta sprite_write+$01
		sty sprite_count

		ldy sprite_x_off
		lda sprite_data+$000,x
		ora (sprite_write),y
		sta (sprite_write),y
		tya
		clc
		adc #$08
		tay
		lda sprite_data+sprite_offset,x
		ora (sprite_write),y
		sta (sprite_write),y

		inx

; Update scanline $01
		ldy sprite_count
		iny
		lda screen_offset_l,y
		sta sprite_write+$00
		lda screen_offset_h,y
		sta sprite_write+$01
		sty sprite_count

		ldy sprite_x_off
		lda sprite_data+$000,x
		ora (sprite_write),y
		sta (sprite_write),y
		tya
		clc
		adc #$08
		tay
		lda sprite_data+sprite_offset,x
		ora (sprite_write),y
		sta (sprite_write),y

		inx

; Update scanline $02
		ldy sprite_count
		iny
		lda screen_offset_l,y
		sta sprite_write+$00
		lda screen_offset_h,y
		sta sprite_write+$01
		sty sprite_count

		ldy sprite_x_off
		lda sprite_data+$000,x
		ora (sprite_write),y
		sta (sprite_write),y
		tya
		clc
		adc #$08
		tay
		lda sprite_data+sprite_offset,x
		ora (sprite_write),y
		sta (sprite_write),y

		inx

; Update scanline $03
		ldy sprite_count
		iny
		lda screen_offset_l,y
		sta sprite_write+$00
		lda screen_offset_h,y
		sta sprite_write+$01
		sty sprite_count

		ldy sprite_x_off
		lda sprite_data+$000,x
		ora (sprite_write),y
		sta (sprite_write),y
		tya
		clc
		adc #$08
		tay
		lda sprite_data+sprite_offset,x
		ora (sprite_write),y
		sta (sprite_write),y

		inx

; Update scanline $04
		ldy sprite_count
		iny
		lda screen_offset_l,y
		sta sprite_write+$00
		lda screen_offset_h,y
		sta sprite_write+$01
		sty sprite_count

		ldy sprite_x_off
		lda sprite_data+$000,x
		ora (sprite_write),y
		sta (sprite_write),y
		tya
		clc
		adc #$08
		tay
		lda sprite_data+sprite_offset,x
		ora (sprite_write),y
		sta (sprite_write),y

		inx

; Update scanline $05
		ldy sprite_count
		iny
		lda screen_offset_l,y
		sta sprite_write+$00
		lda screen_offset_h,y
		sta sprite_write+$01
		sty sprite_count

		ldy sprite_x_off
		lda sprite_data+$000,x
		ora (sprite_write),y
		sta (sprite_write),y
		tya
		clc
		adc #$08
		tay
		lda sprite_data+sprite_offset,x
		ora (sprite_write),y
		sta (sprite_write),y

		inx

; Update scanline $06
		ldy sprite_count
		iny
		lda screen_offset_l,y
		sta sprite_write+$00
		lda screen_offset_h,y
		sta sprite_write+$01
		sty sprite_count

		ldy sprite_x_off
		lda sprite_data+$000,x
		ora (sprite_write),y
		sta (sprite_write),y
		tya
		clc
		adc #$08
		tay
		lda sprite_data+sprite_offset,x
		ora (sprite_write),y
		sta (sprite_write),y

		inx

; Update scanline $07
		ldy sprite_count
		iny
		lda screen_offset_l,y
		sta sprite_write+$00
		lda screen_offset_h,y
		sta sprite_write+$01
		sty sprite_count

		ldy sprite_x_off
		lda sprite_data+$000,x
		ora (sprite_write),y
		sta (sprite_write),y
		tya
		clc
		adc #$08
		tay
		lda sprite_data+sprite_offset,x
		ora (sprite_write),y
		sta (sprite_write),y

		inx

; Update scanline $08
		ldy sprite_count
		iny
		lda screen_offset_l,y
		sta sprite_write+$00
		lda screen_offset_h,y
		sta sprite_write+$01
		sty sprite_count

		ldy sprite_x_off
		lda sprite_data+$000,x
		ora (sprite_write),y
		sta (sprite_write),y
		tya
		clc
		adc #$08
		tay
		lda sprite_data+sprite_offset,x
		ora (sprite_write),y
		sta (sprite_write),y

		inx

; Update scanline $09
		ldy sprite_count
		iny
		lda screen_offset_l,y
		sta sprite_write+$00
		lda screen_offset_h,y
		sta sprite_write+$01
		sty sprite_count

		ldy sprite_x_off
		lda sprite_data+$000,x
		ora (sprite_write),y
		sta (sprite_write),y
		tya
		clc
		adc #$08
		tay
		lda sprite_data+sprite_offset,x
		ora (sprite_write),y
		sta (sprite_write),y

		rts


; Sprite clearing subroutine
; Y register is height
clear_sprite

; Update scanline $00
		lda screen_offset_l,y
		sta sprite_write+$00
		lda screen_offset_h,y
		sta sprite_write+$01
		sty sprite_count

		ldy sprite_x_off
		lda #$00
		sta (sprite_write),y
		tya
		clc
		adc #$08
		tay
		lda #$00
		sta (sprite_write),y

		inx

; Update scanline $01
		ldy sprite_count
		iny
		lda screen_offset_l,y
		sta sprite_write+$00
		lda screen_offset_h,y
		sta sprite_write+$01
		sty sprite_count

		ldy sprite_x_off
		lda #$00
		sta (sprite_write),y
		tya
		clc
		adc #$08
		tay
		lda #$00
		sta (sprite_write),y

		inx

; Update scanline $02
		ldy sprite_count
		iny
		lda screen_offset_l,y
		sta sprite_write+$00
		lda screen_offset_h,y
		sta sprite_write+$01
		sty sprite_count

		ldy sprite_x_off
		lda #$00
		sta (sprite_write),y
		tya
		clc
		adc #$08
		tay
		lda #$00
		sta (sprite_write),y

		inx

; Update scanline $03
		ldy sprite_count
		iny
		lda screen_offset_l,y
		sta sprite_write+$00
		lda screen_offset_h,y
		sta sprite_write+$01
		sty sprite_count

		ldy sprite_x_off
		lda #$00
		sta (sprite_write),y
		tya
		clc
		adc #$08
		tay
		lda #$00
		sta (sprite_write),y

		inx

; Update scanline $04
		ldy sprite_count
		iny
		lda screen_offset_l,y
		sta sprite_write+$00
		lda screen_offset_h,y
		sta sprite_write+$01
		sty sprite_count

		ldy sprite_x_off
		lda #$00
		sta (sprite_write),y
		tya
		clc
		adc #$08
		tay
		lda #$00
		sta (sprite_write),y

		inx

; Update scanline $05
		ldy sprite_count
		iny
		lda screen_offset_l,y
		sta sprite_write+$00
		lda screen_offset_h,y
		sta sprite_write+$01
		sty sprite_count

		ldy sprite_x_off
		lda #$00
		sta (sprite_write),y
		tya
		clc
		adc #$08
		tay
		lda #$00
		sta (sprite_write),y

		inx

; Update scanline $06
		ldy sprite_count
		iny
		lda screen_offset_l,y
		sta sprite_write+$00
		lda screen_offset_h,y
		sta sprite_write+$01
		sty sprite_count

		ldy sprite_x_off
		lda #$00
		sta (sprite_write),y
		tya
		clc
		adc #$08
		tay
		lda #$00
		sta (sprite_write),y

		inx

; Update scanline $07
		ldy sprite_count
		iny
		lda screen_offset_l,y
		sta sprite_write+$00
		lda screen_offset_h,y
		sta sprite_write+$01
		sty sprite_count

		ldy sprite_x_off
		lda #$00
		sta (sprite_write),y
		tya
		clc
		adc #$08
		tay
		lda #$00
		sta (sprite_write),y

		inx

; Update scanline $08
		ldy sprite_count
		iny
		lda screen_offset_l,y
		sta sprite_write+$00
		lda screen_offset_h,y
		sta sprite_write+$01
		sty sprite_count

		ldy sprite_x_off
		lda #$00
		sta (sprite_write),y
		tya
		clc
		adc #$08
		tay
		lda #$00
		sta (sprite_write),y

		inx

; Update scanline $09
		ldy sprite_count
		iny
		lda screen_offset_l,y
		sta sprite_write+$00
		lda screen_offset_h,y
		sta sprite_write+$01
		sty sprite_count

		ldy sprite_x_off
		lda #$00
		sta (sprite_write),y
		tya
		clc
		adc #$08
		tay
		lda #$00
		sta (sprite_write),y

		rts
