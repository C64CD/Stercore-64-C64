; Scroll the playfield attribute RAM (unrolled for speed)
scroll_update

!set line_count=$00
!do {

!set column_count=$00
!do {
	lda attrib_ram+$a5+(line_count*$28)+(column_count+$01)
	sta attrib_ram+$a5+(line_count*$28)+(column_count+$00)

	!set column_count=column_count+$01
} until column_count=$1d

	lda column_buffer+line_count
	sta attrib_ram+(line_count*$28)+$c2

	!set line_count=line_count+$01
} until line_count=$14

; Draw a column in from the current tiles
su_column_draw	ldy tile_count

		lda (tile_read_1),y
		sta column_buffer+$00
		lda (tile_read_2),y
		sta column_buffer+$04
		lda (tile_read_3),y
		sta column_buffer+$08
		lda (tile_read_4),y
		sta column_buffer+$0c
		lda (tile_read_5),y
		sta column_buffer+$10

		tya
		clc
		adc #$04
		tay

		lda (tile_read_1),y
		sta column_buffer+$01
		lda (tile_read_2),y
		sta column_buffer+$05
		lda (tile_read_3),y
		sta column_buffer+$09
		lda (tile_read_4),y
		sta column_buffer+$0d
		lda (tile_read_5),y
		sta column_buffer+$11

		tya
		clc
		adc #$04
		tay

		lda (tile_read_1),y
		sta column_buffer+$02
		lda (tile_read_2),y
		sta column_buffer+$06
		lda (tile_read_3),y
		sta column_buffer+$0a
		lda (tile_read_4),y
		sta column_buffer+$0e
		lda (tile_read_5),y
		sta column_buffer+$12

		tya
		clc
		adc #$04
		tay

		lda (tile_read_1),y
		sta column_buffer+$03
		lda (tile_read_2),y
		sta column_buffer+$07
		lda (tile_read_3),y
		sta column_buffer+$0b
		lda (tile_read_4),y
		sta column_buffer+$0f
		lda (tile_read_5),y
		sta column_buffer+$13

; Count how far into the tile we've got
		ldx tile_count
		inx
		stx tile_count
		cpx #$04
		beq *+$05
		jmp su_cd_exit

; Read the next column of tiles
		ldy #$00
		lda (map_position),y		; tile row $00
		cmp #$ff
		bne su_no_end

; $ff found, so clear map_flag to signal the end has been reached
		lda #$00
		sta map_flag

; Carry on with unpacking the column of tiles
su_no_end	sta tile_read_1+$00
		lda #$00

		asl tile_read_1+$00
		rol
		asl tile_read_1+$00
		rol
		asl tile_read_1+$00
		rol
		asl tile_read_1+$00
		rol
		clc
		adc #>tile_data
		sta tile_read_1+$01

		lda tile_read_1+$00
		clc
		adc #<tile_data
		bcc *+$04
		inc tile_read_1+$01
		sta tile_read_1+$00

		inc map_position+$00
		bne *+$04
		inc map_position+$01

		lda (map_position),y		; tile row $01
		sta tile_read_2+$00
		lda #$00

		asl tile_read_2+$00
		rol
		asl tile_read_2+$00
		rol
		asl tile_read_2+$00
		rol
		asl tile_read_2+$00
		rol
		clc
		adc #>tile_data
		sta tile_read_2+$01

		lda tile_read_2+$00
		clc
		adc #<tile_data
		bcc *+$04
		inc tile_read_2+$01
		sta tile_read_2+$00

		inc map_position+$00
		bne *+$04
		inc map_position+$01

		lda (map_position),y		; tile row $02
		sta tile_read_3+$00
		lda #$00

		asl tile_read_3+$00
		rol
		asl tile_read_3+$00
		rol
		asl tile_read_3+$00
		rol
		asl tile_read_3+$00
		rol
		clc
		adc #>tile_data
		sta tile_read_3+$01

		lda tile_read_3+$00
		clc
		adc #<tile_data
		bcc *+$04
		inc tile_read_3+$01
		sta tile_read_3+$00

		inc map_position+$00
		bne *+$04
		inc map_position+$01

		lda (map_position),y		; tile row $03
		sta tile_read_4+$00
		lda #$00

		asl tile_read_4+$00
		rol
		asl tile_read_4+$00
		rol
		asl tile_read_4+$00
		rol
		asl tile_read_4+$00
		rol
		clc
		adc #>tile_data
		sta tile_read_4+$01

		lda tile_read_4+$00
		clc
		adc #<tile_data
		bcc *+$04
		inc tile_read_4+$01
		sta tile_read_4+$00

		inc map_position+$00
		bne *+$04
		inc map_position+$01

		lda (map_position),y		; tile row $04
		sta tile_read_5+$00
		lda #$00

		asl tile_read_5+$00
		rol
		asl tile_read_5+$00
		rol
		asl tile_read_5+$00
		rol
		asl tile_read_5+$00
		rol
		clc
		adc #>tile_data
		sta tile_read_5+$01

		lda tile_read_5+$00
		clc
		adc #<tile_data
		bcc *+$04
		inc tile_read_5+$01
		sta tile_read_5+$00

		inc map_position+$00
		bne *+$04
		inc map_position+$01

; Reset the tile count
		lda #$00
		sta tile_count

su_cd_exit	rts

; Reset the map readers to the start of their data
scroll_reset	lda #<level_data
		sta map_position+$00
		lda #>level_data
		sta map_position+$01

		lda #<tile_data
		sta tile_read_1+$00
		sta tile_read_2+$00
		sta tile_read_3+$00
		sta tile_read_4+$00
		sta tile_read_5+$00

		lda #>tile_data
		sta tile_read_1+$01
		sta tile_read_2+$01
		sta tile_read_3+$01
		sta tile_read_4+$01
		sta tile_read_5+$01

		lda #$00
		sta tile_count

		lda #$01
		sta map_flag

; Fetch the first column of data
		jsr su_column_draw

		rts
