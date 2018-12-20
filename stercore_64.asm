;
; STERCORE 64
;

; Programming, graphics, sound and other digital atrocities by
; Jason


; A conversion of the simple scrolling shoot 'em up with tile
; compressed backgrounds which was coded for the CSS Crap Game
; Competition 2018 and released at C64CrapDebunk.Wordpress.com

; Notes: this source is formatted for the ACME cross assembler from
; http://sourceforge.net/projects/acme-crossass/
; Compression is handled with Exomizer 2 which can be downloaded at
; http://hem.bredband.net/magli143/exo/

; build.bat will call both to create an assembled file and then the
; crunched release version.


; Select an output filename
		!to "stercore_64.prg",cbm


; Pull in the binary data
		* = $0900
music		!binary "binary\music.prg",,2

		* = $c000
char_data	!binary "binary\characters.chr"

		* = $c400
sprite_data	!binary "binary\sprite_data.raw"


; Constants
cheat		= $00		; set to $01 to disable player collisions

raster_1_pos	= $00
raster_2_pos	= $f5

sprite_offset	= $78		; number of bytes per column of sprite data

; Constants - X positions for the starfield
star_1_x	= $09
star_2_x	= $03
star_3_x	= $19
star_4_x	= $10
star_5_x	= $1c
star_6_x	= $06
star_7_x	= $0b
star_8_x	= $19
star_9_x	= $01
star_10_x	= $1c


; General label assignments
raster_num	= $50		; raster split counter
sync		= $51		; raster sync for runtime code
rt_store_1	= $52		; temporary store for runtime code

flash_timer	= $54		; timer for text flashing
flash_state	= $55		; $00 is normal, $01 is inverse

ctrl_buffer	= $56		; current joystick value
coll_temp	= $57		; collision work space - $04 bytes used

; Labels for the software sprites
sprite_write	= $60		; read/write for soft sprites - $02 bytes used
sprite_count	= $62		; counter?
sprite_x_off	= $63		; X offset of the sprite
bitmap_write	= $64		; read/write for the bitmap - $02 bytes used

; Labels for the background scroller
tile_count	= $70		; column count in the current tile
map_flag	= $71		; end of map marker flag

map_position	= $72		; current read position for the map - $02 bytes used

tile_read_1	= $74		; tile row 1 read position - $02 bytes used
tile_read_2	= $76		; tile row 2 read position - $02 bytes used
tile_read_3	= $78		; tile row 3 read position - $02 bytes used
tile_read_4	= $7a		; tile row 4 read position - $02 bytes used
tile_read_5	= $7c		; tile row 5 read position - $02 bytes used

; Labels for the text plotter
text_read	= $80		; text read position  - $02 bytes used
text_char_read	= $82		; character being copied - $02 bytes used
text_write	= $84		; text write position  - $02 bytes used
text_col_write	= $86		; text colour write position  - $02 bytes used
text_colour	= $88		; text colour
text_length	= $8a		; length of text string


; Labels for the tittles scroller
t_char_buffer	= $90		; work buffer for the scroller - $08 bytes used
t_scrl_count	= $98		; where to fetch scroll text from - $02 bytes used
t_char_read	= $9a		; where to fetch char data from - $02 bytes used
t_scrl_timer	= $9c		; character width timer

t_colour_count	= $9d		; colour counter for the logo effect
t_luma_count	= $9e		; luminance counter for the logo effect

; Where to find the screen
attrib_ram	= $cc00
bitmap_ram	= $e000

; Colour and luma effect work spaces
t_colour_work	= $ff40
t_luma_work	= $ff60


; Add a BASIC startline
		* = $0801
		!word code_start-2
		!byte $40,$00,$9e
		!text "4096"
		!byte $00,$00,$00


; Entry point for the code
		* = $1000

; Stop interrupts, disable the ROMS and set up NMI and IRQ interrupt pointers
code_start	sei

		lda #$35
		sta $01

		lda #<nmi_int
		sta $fffa
		lda #>nmi_int
		sta $fffb

		lda #<irq_int
		sta $fffe
		lda #>irq_int
		sta $ffff

; Set the VIC-II up for a raster IRQ interrupt
		lda #$7f
		sta $dc0d
		sta $dd0d

		lda $dc0d
		lda $dd0d

		lda #raster_1_pos
		sta $d012

		lda #$37
		sta $d011
		lda #$01
		sta $d019
		sta $d01a

; Clear the zero page and initialise some labels
		ldx #$50
		lda #$00
clear_zp	sta $00,x
		inx
		bne clear_zp

		lda #$01
		sta raster_num

; Set up the video registers
		lda #$00
		sta $d020
		sta $d021

		lda #$38
		sta $d018

		lda #$c4
		sta $dd00

; Set up the music driver
		lda #$00
		jsr music+$00

; Restart the interrupts
		cli


; Titles page entry point
titles_init	jsr screen_init

; Titles logo - line 1
		lda #<t_logo_1			; text source
		sta text_read+$00
		lda #>t_logo_1
		sta text_read+$01
		lda #<bitmap_ram+$0668		; screen destination
		sta text_write+$00
		lda #>bitmap_ram+$0668
		sta text_write+$01

		lda #<attrib_ram+$0cd		; colour destination
		sta text_col_write+$00
		lda #>attrib_ram+$0cd
		sta text_col_write+$01
		lda #$f0			; text colour
		sta text_colour

		lda #$1e			; text length
		sta text_length
		jsr text_render

; Titles logo - line 2
		lda #<t_logo_2			; text source
		sta text_read+$00
		lda #>t_logo_2
		sta text_read+$01
		lda #<bitmap_ram+$07a8		; screen destination
		sta text_write+$00
		lda #>bitmap_ram+$07a8
		sta text_write+$01

		lda #<attrib_ram+$0f5		; colour destination
		sta text_col_write+$00
		lda #>attrib_ram+$0f5
		sta text_col_write+$01
		lda #$f0			; text colour
		sta text_colour

		lda #$1e			; text length
		sta text_length
		jsr text_render

; Titles logo - line 3
		lda #<t_logo_3			; text source
		sta text_read+$00
		lda #>t_logo_3
		sta text_read+$01
		lda #<bitmap_ram+$08e8		; screen destination
		sta text_write+$00
		lda #>bitmap_ram+$08e8
		sta text_write+$01

		lda #<attrib_ram+$11d		; colour destination
		sta text_col_write+$00
		lda #>attrib_ram+$11d
		sta text_col_write+$01
		lda #$f0			; text colour
		sta text_colour

		lda #$1e			; text length
		sta text_length
		jsr text_render

; Titles credit text - line 1
		lda #<t_credit_1		; text source
		sta text_read+$00
		lda #>t_credit_1
		sta text_read+$01
		lda #<bitmap_ram+$0ca8		; screen destination
		sta text_write+$00
		lda #>bitmap_ram+$0ca8
		sta text_write+$01

		lda #<attrib_ram+$195		; colour destination
		sta text_col_write+$00
		lda #>attrib_ram+$195
		sta text_col_write+$01
		lda #$60			; text colour
		sta text_colour

		lda #$1e			; text length
		sta text_length
		jsr text_render

; Titles credit text - line 2
		lda #<t_credit_2		; text source
		sta text_read+$00
		lda #>t_credit_2
		sta text_read+$01
		lda #<bitmap_ram+$0de8		; screen destination
		sta text_write+$00
		lda #>bitmap_ram+$0de8
		sta text_write+$01

		lda #<attrib_ram+$1bd		; colour destination
		sta text_col_write+$00
		lda #>attrib_ram+$1bd
		sta text_col_write+$01
		lda #$e0			; text colour
		sta text_colour

		lda #$14			; text length
		sta text_length
		jsr text_render

; Titles credit text - line 3
		lda #<t_credit_3		; text source
		sta text_read+$00
		lda #>t_credit_3
		sta text_read+$01
		lda #<bitmap_ram+$0eb0		; screen destination
		sta text_write+$00
		lda #>bitmap_ram+$0eb0
		sta text_write+$01

		lda #<attrib_ram+$1d6		; colour destination
		sta text_col_write+$00
		lda #>attrib_ram+$1d6
		sta text_col_write+$01
		lda #$30			; text colour
		sta text_colour

		lda #$05			; text length
		sta text_length
		jsr text_render

; Titles credit text - line 4
		lda #<t_credit_4		; text source
		sta text_read+$00
		lda #>t_credit_4
		sta text_read+$01
		lda #<bitmap_ram+$12f8		; screen destination
		sta text_write+$00
		lda #>bitmap_ram+$12f8
		sta text_write+$01

		lda #<attrib_ram+$25f		; colour destination
		sta text_col_write+$00
		lda #>attrib_ram+$25f
		sta text_col_write+$01
		lda #$70			; text colour
		sta text_colour

		lda #$1a			; text length
		sta text_length
		jsr text_render

; Titles credit text - line 5
		lda #<t_credit_5		; text source
		sta text_read+$00
		lda #>t_credit_5
		sta text_read+$01
		lda #<bitmap_ram+$1568		; screen destination
		sta text_write+$00
		lda #>bitmap_ram+$1568
		sta text_write+$01

		lda #<attrib_ram+$2ad		; colour destination
		sta text_col_write+$00
		lda #>attrib_ram+$2ad
		sta text_col_write+$01
		lda #$a0			; text colour
		sta text_colour

		lda #$1e			; text length
		sta text_length
		jsr text_render

; Titles credit text - line 6
		lda #<t_credit_6		; text source
		sta text_read+$00
		lda #>t_credit_6
		sta text_read+$01
		lda #<bitmap_ram+$16a8		; screen destination
		sta text_write+$00
		lda #>bitmap_ram+$16a8
		sta text_write+$01

		lda #<attrib_ram+$2d5		; colour destination
		sta text_col_write+$00
		lda #>attrib_ram+$2d5
		sta text_col_write+$01
		lda #$80			; text colour
		sta text_colour

		lda #$1e			; text length
		sta text_length
		jsr text_render

; Titles press fire message
		lda #<t_text_4			; text source
		sta text_read+$00
		lda #>t_text_4
		sta text_read+$01
		lda #<bitmap_ram+$1a88		; screen destination
		sta text_write+$00
		lda #>bitmap_ram+$1a88
		sta text_write+$01

		lda #<attrib_ram+$351		; colour destination
		sta text_col_write+$00
		lda #>attrib_ram+$351
		sta text_col_write+$01
		lda #$04			; text colour
		sta text_colour

		lda #$16			; text length
		sta text_length
		jsr text_render

; Set up the scroller's colours
		ldx #$00
t_scrl_col_copy	lda t_scrl_c_data,x
		sta attrib_ram+$39d,x
		inx
		cpx #$1e
		bne t_scrl_col_copy

; Reset the logo's colour effects
		lda #$00
		sta t_colour_count
		sta t_luma_count

; Clear the logo's colour and luma buffers
		ldx #$00
t_clr_logo_cl	lda t_colour_data
		sta t_colour_work,x
		lda t_luma_data
		sta t_luma_work,x
		inx
		cpx #$20
		bne t_clr_logo_cl

; Reset the scrolling message
		jsr t_scrl_reset
		lda #$08
		sta t_scrl_timer

		ldx #$00
		txa
t_clr_char	sta t_char_buffer,x
		inx
		cpx #$08
		bne t_clr_char

; Reset the flash timer and state labels
		lda #$00
		sta flash_timer
		sta flash_state


; Titles page
titles_loop	jsr sync_wait

; Update the logo's colour wash effect
		ldx #$00
t_colour_move	lda t_colour_work+$01,x
		sta t_colour_work+$00,x
		inx
		cpx #$1f
		bne t_colour_move

		ldx t_colour_count
		lda t_colour_data,x
		sta t_colour_work+$1f
		inx
		cpx #$37
		bne *+$04
		ldx #$00
		stx t_colour_count

; Update the logo's luma wash effect
		ldx #$1f
t_luma_move	lda t_luma_work+$00,x
		sta t_luma_work+$01,x
		dex
		cpx #$ff
		bne t_luma_move

		ldx t_luma_count
		lda t_luma_data,x
		sta t_luma_work+$00
		inx
		cpx #$80
		bne *+$04
		ldx #$00
		stx t_luma_count

; Merge the logo's colour and luma data
		ldx #$01
t_colour_merge	lda t_colour_work,x
		clc
		adc t_luma_work,x
		tay
		lda t_colour_trans,y
		sta attrib_ram+$0cc,x
		sta attrib_ram+$0f4,x
		sta attrib_ram+$11c,x
		inx
		cpx #$1f
		bne t_colour_merge

; Update the scrolling message
		jsr t_scroller
		jsr t_scroller

; Flash the press fire message
		lda #$f4
		ldx flash_state
		beq *+$04
		lda #$40

		ldx #$00
t_flash_loop	sta attrib_ram+$351,x
		inx
		cpx #$16
		bne t_flash_loop

; Start the game if fire has been pressed
		lda $dc00
		and #$10
		bne titles_loop


; Clear the bitmap RAM
main_init	jsr screen_init

; Reset the background scroll engine
		jsr scroll_reset

; Reset player and bullet co-ordinates
		lda #$1c
		sta player_x
		lda #$27
		sta player_y

		lda #$00
		sta bullet_x
		lda #$64
		sta bullet_y

; Zero the score
		ldx #$00
		txa
score_reset	sta player_score,x
		inx
		cpx #$05
		bne score_reset

; Set the lives counter
		lda #$03
		sta player_lives

; Set the player shield
		lda #$32
		sta player_shield

; Reset the status bar
		jsr status_update


; Main loop
main_loop	jsr sync_wait

; Call the various subroutines that process the game
		jsr sprite_clear

		jsr player_update
		jsr bullet_update

		jsr nasty_update

		jsr sprite_draw

		jsr scroll_update

		jsr bump_score
		jsr status_update

; Check for the player's death flag
		lda player_d_flag
		beq no_death

; Conditional assembly, if cheat is $01 then skip the player collisions
!if cheat=$01 {
		jmp no_death
}

; Set the player shield
		lda #$32
		sta player_shield

; Trigger the player explosion sound
		lda #<plyr_death_sfx
		ldy #>plyr_death_sfx
		ldx #$0e
		jsr music+$06

; Decrease and check player's lives counter
		lda player_lives
		sec
		sbc #$01
		sta player_lives

		bne *+$05
		jmp game_over_init

; Check to see if the end of map flag is set
no_death	lda map_flag
		cmp #$01
		bne game_done_init
		jmp main_loop


; Game completion
game_done_init	jsr status_update

; Completion text - line 1
		lda #<completion_1		; text source
		sta text_read+$00
		lda #>completion_1
		sta text_read+$01
		lda #<bitmap_ram+$0e10		; screen destination
		sta text_write+$00
		lda #>bitmap_ram+$0e10
		sta text_write+$01

		lda #<attrib_ram+$1c2		; colour destination
		sta text_col_write+$00
		lda #>attrib_ram+$1c2
		sta text_col_write+$01
		lda #$05			; text colour
		sta text_colour

		lda #$14			; text length
		sta text_length
		jsr text_render

; Completion text - line 2
		lda #<completion_2		; text source
		sta text_read+$00
		lda #>completion_2
		sta text_read+$01
		lda #<bitmap_ram+$10b0		; screen destination
		sta text_write+$00
		lda #>bitmap_ram+$10b0
		sta text_write+$01

		lda #<attrib_ram+$216		; colour destination
		sta text_col_write+$00
		lda #>attrib_ram+$216
		sta text_col_write+$01
		lda #$0d			; text colour
		sta text_colour

		lda #$0c			; text length
		sta text_length
		jsr text_render

; Play the completion sound effect
		lda #<completion_sfx
		ldy #>completion_sfx
		ldx #$0e
		jsr music+$06

; Flash the completion message for a few seconds
		lda #$a0
		sta rt_store_1

		lda #$00
		sta flash_timer
		sta flash_state

game_done_loop	jsr sync_wait

		jsr gd_text_flash

		dec rt_store_1
		bne game_done_loop

; Press fire text
		lda #<fire_txt			; text source
		sta text_read+$00
		lda #>fire_txt
		sta text_read+$01
		lda #<bitmap_ram+$1bc0		; screen destination
		sta text_write+$00
		lda #>bitmap_ram+$1bc0
		sta text_write+$01

		lda #<attrib_ram+$378		; colour destination
		sta text_col_write+$00
		lda #>attrib_ram+$378
		sta text_col_write+$01
		lda #$04			; text colour
		sta text_colour

		lda #$18			; text length
		sta text_length
		jsr text_render

; Wait for a fire press, then flip back to the titles
game_done_loop2	jsr sync_wait

		jsr gd_text_flash

		lda $dc00
		and #$10
		bne game_done_loop2

		jmp titles_init

; Flash both of the completion messages
gd_text_flash	lda #$05
		ldx flash_state
		beq *+$04
		lda #$50

		ldx #$00
gdf_loop_1	sta attrib_ram+$1c2,x
		inx
		cpx #$14
		bne gdf_loop_1

		lda #$0d
		ldx flash_state
		beq *+$04
		lda #$d0

		ldx #$00
gdf_loop_2	sta attrib_ram+$216,x
		inx
		cpx #$0c
		bne gdf_loop_2

		rts


; Game over
game_over_init	jsr status_update

; Game over text
		lda #<game_over_text		; text source
		sta text_read+$00
		lda #>game_over_text
		sta text_read+$01
		lda #<bitmap_ram+$0f70		; screen destination
		sta text_write+$00
		lda #>bitmap_ram+$0f70
		sta text_write+$01

		lda #<attrib_ram+$1ee		; colour destination
		sta text_col_write+$00
		lda #>attrib_ram+$1ee
		sta text_col_write+$01
		lda #$02			; text colour
		sta text_colour

		lda #$0c			; text length
		sta text_length
		jsr text_render

; Play the game over sound effect
		lda #<game_over_sfx
		ldy #>game_over_sfx
		ldx #$0e
		jsr music+$06

; Wait for a couple of seconds
		lda #$c8
		sta rt_store_1

		lda #$00
		sta flash_timer
		sta flash_state

game_over_loop	jsr sync_wait

		lda #$02
		ldx flash_state
		beq *+$04
		lda #$20

		ldx #$00
go_flash	sta attrib_ram+$1ee,x
		inx
		cpx #$0c
		bne go_flash

		dec rt_store_1
		bne game_over_loop

		jmp titles_init


; Scroller for the titles
t_scroller	ldx #$00

t_mover		asl t_char_buffer,x
		rol bitmap_ram+$1dd8,x
		rol bitmap_ram+$1dd0,x
		rol bitmap_ram+$1dc8,x
		rol bitmap_ram+$1dc0,x
		rol bitmap_ram+$1db8,x
		rol bitmap_ram+$1db0,x
		rol bitmap_ram+$1da8,x
		rol bitmap_ram+$1da0,x

		rol bitmap_ram+$1d98,x
		rol bitmap_ram+$1d90,x
		rol bitmap_ram+$1d88,x
		rol bitmap_ram+$1d80,x
		rol bitmap_ram+$1d78,x
		rol bitmap_ram+$1d70,x
		rol bitmap_ram+$1d68,x
		rol bitmap_ram+$1d60,x

		rol bitmap_ram+$1d58,x
		rol bitmap_ram+$1d50,x
		rol bitmap_ram+$1d48,x
		rol bitmap_ram+$1d40,x
		rol bitmap_ram+$1d38,x
		rol bitmap_ram+$1d30,x
		rol bitmap_ram+$1d28,x
		rol bitmap_ram+$1d20,x

		rol bitmap_ram+$1d18,x
		rol bitmap_ram+$1d10,x
		rol bitmap_ram+$1d08,x
		rol bitmap_ram+$1d00,x
		rol bitmap_ram+$1cf8,x
		rol bitmap_ram+$1cf0,x
		rol bitmap_ram+$1ce8,x
		rol bitmap_ram+$1ce0,x
		inx
		cpx #$08
		bne t_mover

; Check to see if a new character is due
		ldx t_scrl_timer
		dex
		cpx #$ff
		bne t_no_new_char

; If so, read the scrolltext
t_mread		ldy #$00
		lda (t_scrl_count),y
		cmp #$ff
		bne t_okay

		jsr t_scrl_reset
		jmp t_mread

; Write the new character to the work buffer
t_okay		sta t_char_read+$00
		lda #$00
		asl t_char_read+$00
		rol
		asl t_char_read+$00
		rol
		asl t_char_read+$00
		rol
		clc
		adc #>char_data
		sta t_char_read+$01

		ldy #$00
t_fetch_char	lda (t_char_read),y
		sta t_char_buffer,y
		iny
		cpy #$08
		bne t_fetch_char

; Bump the text counter to the next character
		inc t_scrl_count+$00
		bne *+$04
		inc t_scrl_count+$01

; Skip to here if a new character isn't needed
		ldx #$08
t_no_new_char	stx t_scrl_timer
		rts

; Reset the scroll reader
t_scrl_reset	lda #<t_scrl_text
		sta t_scrl_count+$00
		lda #>t_scrl_text
		sta t_scrl_count+$01

		rts


; Everything to do with updating the player
player_update	lda $dc00
		sta ctrl_buffer

; Check for joystick up
pu_up		lda ctrl_buffer
		and #$01
		bne pu_down

		lda player_y
		sec
		sbc #$01
		cmp #$04
		bcs *+$04
		lda #$04
		sta player_y

; Check for joystick down
pu_down		lda ctrl_buffer
		and #$02
		bne pu_left

		lda player_y
		clc
		adc #$01
		cmp #$50
		bcc *+$04
		lda #$4f
		sta player_y

; Check for joystick left
pu_left		lda ctrl_buffer
		and #$04
		bne pu_right

		lda player_x
		sec
		sbc #$01
		cmp #$04
		bcs *+$04
		lda #$04
		sta player_x

; Check for joystick right
pu_right	lda ctrl_buffer
		and #$08
		bne pu_fire

		lda player_x
		clc
		adc #$01
		cmp #$70
		bcc *+$04
		lda #$6f
		sta player_x

; Check for joystick fire
pu_fire		lda ctrl_buffer
		and #$10
		bne pu_bullet_out

; Launch the player bullet (if it's not busy)
		lda bullet_y
		cmp #$64
		bne pu_bullet_out

; Set bullet X, Y and duration
		lda player_x
		sta bullet_x
		lda player_y
		sta bullet_y

		lda #$0e
		sta bullet_duration

; Set the SFX driver if it's not busy
pu_bullet_out

; Player to nasty collision checks
; Offset the player position to work out if it's collided
		lda player_x
		sec
		sbc #$03
		sta coll_temp+$00
		clc
		adc #$07
		sta coll_temp+$01

		lda player_y
		sec
		sbc #$03
		sta coll_temp+$02
		clc
		adc #$07
		sta coll_temp+$03

; Reset the death flag
		lda #$00
		sta player_d_flag

; Enemy 1 to player collision check
		lda enemy_1_x
		cmp coll_temp+$00
		bcc pu_e1_coll_skip
		cmp coll_temp+$01
		bcs pu_e1_coll_skip

		lda enemy_1_y
		cmp coll_temp+$02
		bcc pu_e1_coll_skip
		cmp coll_temp+$03
		bcs pu_e1_coll_skip

; Enemy 1 has collided, so react accordingly
		lda #$7f
		sta enemy_1_x
		lda enemy_1_y
		clc
		adc #$10
		sta enemy_1_y

; Flag that the player has collided
		inc player_d_flag
		jmp pu_e4_coll_skip

; Enemy 2 to player collision check
pu_e1_coll_skip	lda enemy_2_x
		cmp coll_temp+$00
		bcc pu_e2_coll_skip
		cmp coll_temp+$01
		bcs pu_e2_coll_skip

		lda enemy_2_y
		cmp coll_temp+$02
		bcc pu_e2_coll_skip
		cmp coll_temp+$03
		bcs pu_e2_coll_skip

; Enemy 2 has collided, so react accordingly
		lda #$7f
		sta enemy_2_x
		lda enemy_2_y
		clc
		adc #$f0
		sta enemy_2_y

; Flag that the player has collided
		inc player_d_flag
		jmp pu_e4_coll_skip

; Enemy 3 to player collision check
pu_e2_coll_skip	lda enemy_3_x
		cmp coll_temp+$00
		bcc pu_e3_coll_skip
		cmp coll_temp+$01
		bcs pu_e3_coll_skip

		lda enemy_3_y
		cmp coll_temp+$02
		bcc pu_e3_coll_skip
		cmp coll_temp+$03
		bcs pu_e3_coll_skip

; Enemy 3 has collided, so react accordingly
		lda #$7f
		sta enemy_3_x
		lda enemy_3_y
		clc
		adc #$18
		sta enemy_3_y

; Flag that the player has collided
		inc player_d_flag
		jmp pu_e4_coll_skip

; Enemy 4 to player collision check
pu_e3_coll_skip	lda enemy_4_x
		cmp coll_temp+$00
		bcc pu_e4_coll_skip
		cmp coll_temp+$01
		bcs pu_e4_coll_skip

		lda enemy_4_y
		cmp coll_temp+$02
		bcc pu_e4_coll_skip
		cmp coll_temp+$03
		bcs pu_e4_coll_skip

; Enemy 4 has collided, so react accordingly
		lda #$7f
		sta enemy_4_x
		lda enemy_4_y
		clc
		adc #$e8
		sta enemy_4_y

; Flag that the player has collided
		inc player_d_flag
		jmp pu_e4_coll_skip

; Update the player shield
pu_e4_coll_skip	lda player_shield
		beq pu_exit

; If shield is on, decrease it and zero the death flag
		dec player_shield

		lda #$00
		sta player_d_flag

pu_exit		rts


; Update the player bullet's position and check for right border
bullet_update	lda bullet_x
		clc
		adc #$04
		and #$7f
		sta bullet_x
		cmp #$7b
		bcs bu_remove

; Update the bullet's counter and check if it has expired
		dec bullet_duration
		beq bu_remove

		jmp bu_okay

; Remove the bullet
bu_remove	lda #$00
		sta bullet_x
		lda #$64
		sta bullet_y

; Reset the score bonus
bu_okay		lda #$00
		sta score_bonus

; Bullet to nasty collision checks
		lda bullet_y
		cmp #$64
		bne *+$05
		jmp bu_e4_coll_skip

; Offset the bullet position to work out if it's collided
		sec
		sbc #$06
		sta coll_temp+$02
		clc
		adc #$0d
		sta coll_temp+$03

		lda bullet_x
		sec
		sbc #$06
		sta coll_temp+$00
		clc
		adc #$0d
		sta coll_temp+$01

; Enemy 1 to bullet collision check
		lda enemy_1_x
		cmp coll_temp+$00
		bcc bu_e1_coll_skip
		cmp coll_temp+$01
		bcs bu_e1_coll_skip

		lda enemy_1_y
		cmp coll_temp+$02
		bcc bu_e1_coll_skip
		cmp coll_temp+$03
		bcs bu_e1_coll_skip

; Enemy 1 has been shot, so react accordingly
		lda #$00
		sta enemy_1_x

		lda #$64
		sta bullet_y

		lda score_bonus
		clc
		adc #$05
		sta score_bonus

		jmp bu_e4_coll_skip

; Enemy 2 to bullet collision check
bu_e1_coll_skip	lda enemy_2_x
		cmp coll_temp+$00
		bcc bu_e2_coll_skip
		cmp coll_temp+$01
		bcs bu_e2_coll_skip

		lda enemy_2_y
		cmp coll_temp+$02
		bcc bu_e2_coll_skip
		cmp coll_temp+$03
		bcs bu_e2_coll_skip

; Enemy 2 has been shot, so react accordingly
		lda #$00
		sta enemy_2_x

		lda #$64
		sta bullet_y

		lda score_bonus
		clc
		adc #$05
		sta score_bonus

		jmp bu_e4_coll_skip

; Enemy 3 to bullet collision check
bu_e2_coll_skip	lda enemy_3_x
		cmp coll_temp+$00
		bcc bu_e3_coll_skip
		cmp coll_temp+$01
		bcs bu_e3_coll_skip

		lda enemy_3_y
		cmp coll_temp+$02
		bcc bu_e3_coll_skip
		cmp coll_temp+$03
		bcs bu_e3_coll_skip

; Enemy 3 has been shot, so react accordingly
		lda #$00
		sta enemy_3_x

		lda #$64
		sta bullet_y

		lda score_bonus
		clc
		adc #$07
		sta score_bonus

		jmp bu_e4_coll_skip

; Enemy 4 to bullet collision check
bu_e3_coll_skip	lda enemy_4_x
		cmp coll_temp+$00
		bcc bu_e4_coll_skip
		cmp coll_temp+$01
		bcs bu_e4_coll_skip

		lda enemy_4_y
		cmp coll_temp+$02
		bcc bu_e4_coll_skip
		cmp coll_temp+$03
		bcs bu_e4_coll_skip

; Enemy 4 has been shot, so react accordingly
		lda #$00
		sta enemy_4_x

		lda #$64
		sta bullet_y

		lda score_bonus
		clc
		adc #$07
		sta score_bonus

; Jumped to if the bullet is inactive
bu_e4_coll_skip	rts


; Update enemy 1 position
nasty_update	dec enemy_1_x
		inc enemy_1_y

; Update enemy 2 position
		dec enemy_2_x
		dec enemy_2_y

; Update enemy 3 position
		dec enemy_3_x
		dec enemy_3_x
		inc enemy_3_y

; Update enemy 4 position
		dec enemy_4_x
		dec enemy_4_x
		dec enemy_4_y

; Nudge the sprites around in Y if they're at X $7e/7f
		lda enemy_1_x
		cmp #$7e
		bcc nu_nudge_2

		lda enemy_1_y
		clc
		adc #$30
		sta enemy_1_y

nu_nudge_2	lda enemy_2_x
		cmp #$7e
		bcc nu_nudge_3

		lda enemy_2_y
		clc
		adc #$cd
		sta enemy_2_y

nu_nudge_3	lda enemy_3_x
		cmp #$7e
		bcc nu_nudge_4

		lda enemy_3_y
		clc
		adc #$1d
		sta enemy_3_y

nu_nudge_4	lda enemy_4_x
		cmp #$7e
		bcc nu_nudge_out

		lda enemy_4_y
		clc
		adc #$dd
		sta enemy_4_y

nu_nudge_out

; Make sure none of the X or Y positions are over $7f
		ldx #$00
nu_7f_clip	lda enemy_1_x,x
		and #$7f
		sta enemy_1_x,x
		inx
		cpx #$08
		bne nu_7f_clip

; Check to see if a nasty is exploding to trigger a sound
		lda score_bonus
		beq nu_exit

		lda #<enemy_death_sfx
		ldy #>enemy_death_sfx
		ldx #$0e
		jsr music+$06

nu_exit		rts


; Set up the screen
screen_init	lda #<(bitmap_ram)
		sta bitmap_write+$00
		lda #>(bitmap_ram)
		sta bitmap_write+$01

bitmap_clear	ldy #$00
		tya
bc_loop		sta (bitmap_write),y
		iny
		bne bc_loop

		inc bitmap_write+$01
		lda bitmap_write+$01
		cmp #$fe
		bne bitmap_clear

; Reset the attribute RAM
		ldx #$00
		txa
attrib_clear	sta attrib_ram+$000,x
		sta attrib_ram+$100,x
		sta attrib_ram+$200,x
		sta attrib_ram+$2e8,x
		inx
		bne attrib_clear

; Debounce the fire button (screen is blank at this point)
fire_debounce	lda $dc00
		and #$10
		beq fire_debounce

; Continue with initialising the display
		ldx #$00
attrib_set	lda #$e6
		sta attrib_ram+$005,x
		sta attrib_ram+$02d,x
		sta attrib_ram+$055,x
		sta attrib_ram+$07d,x

		lda #$f0
		sta attrib_ram+$0a5,x
		sta attrib_ram+$0cd,x
		sta attrib_ram+$0f5,x
		sta attrib_ram+$11d,x
		sta attrib_ram+$145,x
		sta attrib_ram+$16d,x
		sta attrib_ram+$195,x
		sta attrib_ram+$1bd,x

		sta attrib_ram+$1e5,x
		sta attrib_ram+$20d,x
		sta attrib_ram+$235,x
		sta attrib_ram+$25d,x
		sta attrib_ram+$285,x
		sta attrib_ram+$2ad,x
		sta attrib_ram+$2d5,x
		sta attrib_ram+$2fd,x

		sta attrib_ram+$325,x
		sta attrib_ram+$34d,x
		sta attrib_ram+$375,x
		sta attrib_ram+$39d,x
		inx
		cpx #$1e
		bne attrib_set

; Status text - score
		lda #<status_text_1		; text source
		sta text_read+$00
		lda #>status_text_1
		sta text_read+$01
		lda #<bitmap_ram+$0170		; screen destination
		sta text_write+$00
		lda #>bitmap_ram+$0170
		sta text_write+$01

		lda #<attrib_ram+$02e		; colour destination
		sta text_col_write+$00
		lda #>attrib_ram+$02e
		sta text_col_write+$01
		lda #$46			; text colour
		sta text_colour

		lda #$05			; text length
		sta text_length
		jsr text_render

; Status text - lives
		lda #<status_text_3		; text source
		sta text_read+$00
		lda #>status_text_3
		sta text_read+$01
		lda #<bitmap_ram+$0228		; screen destination
		sta text_write+$00
		lda #>bitmap_ram+$0228
		sta text_write+$01

		lda #<attrib_ram+$045		; colour destination
		sta text_col_write+$00
		lda #>attrib_ram+$045
		sta text_col_write+$01
		lda #$46			; text colour
		sta text_colour

		lda #$05			; text length
		sta text_length
		jsr text_render

; Status bar logo - line 1
		lda #<status_logo_1		; text source
		sta text_read+$00
		lda #>status_logo_1
		sta text_read+$01
		lda #<bitmap_ram+$01a0		; screen destination
		sta text_write+$00
		lda #>bitmap_ram+$01a0
		sta text_write+$01

		lda #<attrib_ram+$034		; colour destination
		sta text_col_write+$00
		lda #>attrib_ram+$034
		sta text_col_write+$01
		lda #$36			; text colour
		sta text_colour

		lda #$10			; text length
		sta text_length
		jsr text_render

; Status bar logo - line 2
		lda #<status_logo_2		; text source
		sta text_read+$00
		lda #>status_logo_2
		sta text_read+$01
		lda #<bitmap_ram+$02e0		; screen destination
		sta text_write+$00
		lda #>bitmap_ram+$02e0
		sta text_write+$01

		lda #<attrib_ram+$05c		; colour destination
		sta text_col_write+$00
		lda #>attrib_ram+$05c
		sta text_col_write+$01
		lda #$e6			; text colour
		sta text_colour

		lda #$10			; text length
		sta text_length
		jsr text_render

; Update the status area (called from multiple places)
status_update

; Update the score counter before it's rendered
		ldx #$00
su_score_upd	lda player_score,x
		clc
		adc #$30
		sta status_text_2,x
		inx
		cpx #$05
		bne su_score_upd

; Update the lives counter before it's rendered
		lda player_lives
		clc
		adc #$30
		sta status_text_4

; Plot the score counter onto the status bar
		lda #<status_text_2		; text source
		sta text_read+$00
		lda #>status_text_2
		sta text_read+$01
		lda #<bitmap_ram+$02b0		; screen destination
		sta text_write+$00
		lda #>bitmap_ram+$02b0
		sta text_write+$01

		lda #<attrib_ram+$056		; colour destination
		sta text_col_write+$00
		lda #>attrib_ram+$056
		sta text_col_write+$01
		lda #$56			; text colour
		sta text_colour

		lda #$05			; text length
		sta text_length
		jsr text_render

; Plot the lives counter onto the status bar
		lda #<status_text_4		; text source
		sta text_read+$00
		lda #>status_text_4
		sta text_read+$01
		lda #<bitmap_ram+$0388		; screen destination
		sta text_write+$00
		lda #>bitmap_ram+$0388
		sta text_write+$01

		lda #<attrib_ram+$071		; colour destination
		sta text_col_write+$00
		lda #>attrib_ram+$071
		sta text_col_write+$01
		lda #$56			; text colour
		sta text_colour

		lda #$01			; text length
		sta text_length
		jsr text_render

		rts


; Write a line of text into the bitmap with attribute colours
text_render	ldy #$00
		lda (text_read),y

		sta text_char_read+$00
		lda #$00
		asl text_char_read+$00
		rol
		asl text_char_read+$00
		rol
		asl text_char_read+$00
		rol
		clc
		adc #>char_data
		sta text_char_read+$01

; Copy the character
		ldy #$00
tr_char_copy	lda (text_char_read),y
		sta (text_write),y
		iny
		cpy #$08
		bne tr_char_copy

; Render the colour
		ldy #$00
		lda text_colour
		sta (text_col_write),y

; Bump everything to the next byte
		inc text_read+$00
		bne *+$04
		inc text_read+$01

		lda text_write+$00
		clc
		adc #$08
		bcc *+$04
		inc text_write+$01
		sta text_write+$00

		inc text_col_write+$00
		bne *+$04
		inc text_col_write+$01

; Decrease the length counter and wrap if it's not zero
		dec text_length
		bne text_render

		rts


; Add to the player's score
bump_score	lda score_bonus
		beq bs_exit
		tay

bs_outer_loop	ldx #$03
bs_loop		lda player_score,x
		clc
		adc #$01
		sta player_score,x

		cmp #$0a
		bcc bs_skip

		lda #$00
		sta player_score,x

		dex
		cpx #$ff
		bne bs_loop

bs_skip		dey
		bne bs_outer_loop

bs_exit		rts


; Wait for the end of the screen
sync_wait	lda #$00
		sta sync

sw_loop		cmp sync
		beq sw_loop
		rts


; Bring in the background scroller
		!src "includes/scroll_code.asm"

; Bring in the software sprite code and data
		!src "includes/sprite_code.asm"


; IRQ interrupt handler
irq_int		pha
		txa
		pha
		tya
		pha

		lda $d019
		and #$01
		sta $d019
		bne int_go
		jmp irq_exit

; An interrupt has triggered
int_go		lda raster_num

		cmp #$02
		bne *+$05
		jmp irq_rout2


; Raster split 1
irq_rout1

; Update the flash timer
		ldx flash_timer
		inx
		cpx #$10
		bcc ft_xb

		lda flash_state
		clc
		adc #$01
		and #$01
		sta flash_state

		ldx #$00
ft_xb		stx flash_timer

; Play the music
		jsr music+$03

; Set interrupt handler for split 2
		lda #$02
		sta raster_num
		lda #raster_2_pos
		sta $d012

; Exit IRQ interrupt
		jmp irq_exit


; Raster split 2
irq_rout2	lda #$01
		sta sync

; Set interrupt handler for split 1
		lda #$01
		sta raster_num
		lda #raster_1_pos
		sta $d012

; Restore registers and exit IRQ interrupt
irq_exit	pla
		tay
		pla
		tax
		pla
nmi_int		rti


; Low bytes for offsets to the start of each scanline within RAM
; (Uses a page of the cassette buffer for run off)
screen_offset_l	!byte $40,$40,$40,$40,$40,$40,$40,$40

!set cline_count=$04
!do {

!set sline_count=$00
!do {
		!byte <(bitmap_ram+$20+(cline_count*$140)+sline_count)

		!set sline_count=sline_count+$01
} until sline_count=$08

		!set cline_count=cline_count+$01
} until cline_count=$18

		!byte $40,$40,$40,$40,$40,$40,$40,$40
		!byte $40,$40,$40,$40,$40,$40,$40,$40
		!byte $40,$40,$40,$40,$40,$40,$40,$40
		!byte $40,$40,$40,$40,$40,$40,$40,$40
		!byte $40,$40,$40,$40,$40,$40,$40,$40
		!byte $40,$40,$40,$40,$40,$40,$40,$40
		!byte $40,$40,$40,$40,$40,$40,$40,$40
		!byte $40,$40,$40,$40,$40,$40,$40,$40

		!byte $40,$40,$40,$40,$40,$40,$40,$40
		!byte $40,$40,$40,$40,$40,$40,$40,$40
		!byte $40,$40,$40,$40,$40,$40,$40,$40
		!byte $40,$40,$40,$40,$40,$40,$40,$40

; High bytes for offsets to the start of each scanline within RAM
; (Uses a page of the cassette buffer for run off)
screen_offset_h	!byte $03,$03,$03,$03,$03,$03,$03,$03

!set cline_count=$04
!do {

!set sline_count=$00
!do {
		!byte >(bitmap_ram+$20+(cline_count*$140)+sline_count)

		!set sline_count=sline_count+$01
} until sline_count=$08

		!set cline_count=cline_count+$01
} until cline_count=$18

		!byte $03,$03,$03,$03,$03,$03,$03,$03
		!byte $03,$03,$03,$03,$03,$03,$03,$03
		!byte $03,$03,$03,$03,$03,$03,$03,$03
		!byte $03,$03,$03,$03,$03,$03,$03,$03
		!byte $03,$03,$03,$03,$03,$03,$03,$03
		!byte $03,$03,$03,$03,$03,$03,$03,$03
		!byte $03,$03,$03,$03,$03,$03,$03,$03
		!byte $03,$03,$03,$03,$03,$03,$03,$03

		!byte $03,$03,$03,$03,$03,$03,$03,$03
		!byte $03,$03,$03,$03,$03,$03,$03,$03
		!byte $03,$03,$03,$03,$03,$03,$03,$03
		!byte $03,$03,$03,$03,$03,$03,$03,$03


; Multiply by $0a table, used to select sprite definitions
multi_0a	!byte $00,$0a,$14,$1e,$28,$32,$3c,$46
		!byte $50,$5a,$64,$6e


; Labels for player status
player_score	!byte $05,$03,$02,$08,$00
player_lives	!byte $03
score_bonus	!byte $00

player_shield	!byte $00
player_d_flag	!byte $00

; Player's on-screen position
player_x	!byte $00
player_y	!byte $00

; Player bullet's on-screen position
bullet_x	!byte $00
bullet_y	!byte $20
bullet_duration	!byte $00

; Enemy on-screen positions
enemy_1_x	!byte $20
enemy_1_y	!byte $10

enemy_2_x	!byte $34
enemy_2_y	!byte $2b

enemy_3_x	!byte $5b
enemy_3_y	!byte $36

enemy_4_x	!byte $6f
enemy_4_y	!byte $48


; Columm buffer for the background scroller
column_buffer	!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00


; Status bar logo
status_logo_1	!byte $60,$61,$62,$63,$64,$65,$66,$67
		!byte $68,$69,$6a,$6b,$66,$67,$64,$65

status_logo_2	!byte $70,$71,$72,$73,$74,$75,$76,$77
		!byte $78,$79,$7a,$7b,$76,$77,$74,$75

; Status bar text
status_text_1	!scr "Score"
status_text_2	!scr "     "
status_text_3	!scr "Lives"
status_text_4	!scr " "


; Titles logo data
t_logo_1	!byte $6e,$5c,$5c,$5d,$5b,$5e,$5d,$5d
		!byte $6e,$5c,$5c,$6f,$6e,$5c,$5c,$6e
		!byte $5c,$5c,$6f,$6e,$5c,$5c,$6f,$6e
		!byte $5c,$5c,$6e,$5c,$5c,$6f

t_logo_2	!byte $7e,$5c,$5c,$6f,$20,$1c,$20,$20
		!byte $1e,$5c,$5c,$1f,$1c,$20,$20,$1c
		!byte $20,$20,$00,$1c,$20,$20,$1c,$1c
		!byte $20,$20,$1e,$5c,$5c,$1f

t_logo_3	!byte $5f,$5c,$5c,$7f,$20,$1d,$20,$20
		!byte $7e,$5c,$5c,$5f,$1d,$20,$20,$7e
		!byte $5c,$5c,$7f,$7e,$5c,$5c,$7f,$1d
		!byte $20,$20,$7e,$5c,$5c,$5f

; Titles logo effect colour data
t_colour_data	!byte $0d,$0d,$0d,$0c,$0d,$0c,$0c,$0c
		!byte $0b,$0c,$0b,$0b,$0b,$0a,$0b,$0a
		!byte $0a,$0a,$09,$0a,$09,$09,$09,$00
		!byte $09,$00,$00,$00,$01,$00,$01,$01
		!byte $01,$02,$01,$02,$02,$02,$03,$02
		!byte $03,$03,$03,$04,$03,$04,$04,$04
		!byte $05,$04,$05,$05,$05,$0d,$05

; Titles logo effect luma data
t_luma_data	!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00

		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$01,$00,$00
		!byte $01,$00,$01,$01,$00,$01,$01,$01
		!byte $01,$02,$01,$01,$02,$01,$02,$02
		!byte $01,$02,$02,$02,$02,$01,$02,$02
		!byte $01,$02,$01,$02,$01,$01,$01,$01
		!byte $00,$01,$01,$00,$01,$00,$00,$01

; Titles logo colour translation data
t_colour_trans	!byte $00,$60,$b0,$40,$e0,$30,$d0,$10
		!byte $00,$90,$20,$80,$a0,$f0,$70,$10

; Titles text
t_credit_1	!scr "Design, code, graphics, sounds"
t_credit_2	!scr "and other gubbins by"
t_credit_3	!scr "Jason"

t_credit_4	!scr "A C64CD Studios Production"
t_credit_5	!scr "Converted over to the C64 from"
t_credit_6	!scr "the Spectrum CSSCGC 2018 entry"

t_text_4	!scr " Press Fire To Start! "

; Titles scroller text
t_scrl_text	!scr "Hello and welcome to"
		!scr "        "

		!scr "-=- STERCORE 64 -=-"
		!scr "        "

		!scr "A scrolling shoot 'em up from the C64CD "
		!scr "software mines which was originally written "
		!scr "for the Sinclair Spectrum and released "
		!scr "during the CSSCGC 2018"
		!scr "        "

		!scr "Programming, graphics, sound, general "
		!scr "data wrangling and tea making by "
		!scr "Jason"
		!scr "        "

		!scr "Launch your battered spaceship and fly "
		!scr "headlong into a brightly coloured world "
		!scr "filled with dumb enemies that need to "
		!scr "be obliterated for... reasons?"
		!scr "        "

		!scr "It's a shoot 'em up, nobody really reads "
		!scr "the instructions for these things so just "
		!scr "start the game and go cause havoc already!"
		!scr "        "

		!scr "100 percent machine washable (apart from "
		!scr "the bits that aren't) - this game is "
		!scr "fully compatible with TheC64 Mini and "
		!scr "C64DTV2...  probably."
		!scr "        "

		!scr "C64CD greetings blast out towards:  "
		!scr "1001 Crew, "
		!scr "Ash And Dave, "
		!scr "Black Bag, "
		!scr "Copy Service Stuttgart, "
		!scr "Borderzone Dezign Team, "
		!scr "Dynamic Duo, "

		!scr "Four Horsemen Of The Apocalypse, "
		!scr "Happy Demomaker, "
		!scr "Harlow Cracking Service, "
		!scr "High-tech Team, "
		!scr "Ikari, "
		!scr "Jewels, "

		!scr "Kernal, "
		!scr "Laxity, "
		!scr "Mean Team, "
		!scr "Paul, Shandor and Matt, "
		!scr "Pulse Productions, "
		!scr "Reset 86, "

		!scr "Rob Hubbard, "
		!scr "Scoop, "
		!scr "Slipstream, "
		!scr "Stoat And Tim, "
		!scr "Tangent, "
		!scr "Thalamus, "

		!scr "The Commandos, "
		!scr "The GPS, "
		!scr "The Six Pack, "
		!scr "We Music, "
		!scr "Xess, "
		!scr "Yak, "

		!scr "and Yeti Factories."
		!scr "      "

		!scr "And of course the now traditional anti-greeting "
		!scr "to C64hater because we might as well whilst "
		!scr "here..."
		!scr "        "

		!scr "And that's everything sorted, so here we are "
		!scr "signing off on 2018-12-20 - goodbye for now "
		!scr "and enjoy shooting at things... .. .  ."
		!scr "        "

		!scr $ff		; end of text marker

; Colours for the titles scroller
t_scrl_c_data	!byte $b6,$46,$e6,$36,$d6,$16,$16,$16
		!byte $16,$16,$16,$16,$16,$16,$16,$16
		!byte $16,$16,$16,$16,$16,$16,$16,$16
		!byte $16,$d6,$36,$e6,$46,$b6


; Press Fire prompt text
fire_txt	!scr " Press Fire To Continue "


; Completion text
completion_1	!scr " Mission Completed! "
completion_2	!scr " Well Done! "


; Game over text
game_over_text	!scr " Game Over! "


; Sound effect data
plyr_death_sfx	!byte $00,$fb,$08,$b8,$81

		!byte $a4,$41,$a0,$b4,$81,$98,$92,$9c
		!byte $90,$95,$9e,$92,$80,$94,$8f,$8e
		!byte $8d,$8c,$8d,$8e,$8f,$8e,$8d,$8c
		!byte $8d,$9e,$92,$80,$94,$8f,$8e,$8d
		!byte $8c,$8d,$8e,$8f,$8e,$8d,$8c,$8d
		!byte $00

enemy_death_sfx	!byte $00,$fa,$08,$b8,$81

		!byte $a4,$41,$a8,$bc,$81,$a0,$9a,$a4
		!byte $98,$9d,$a6,$9a,$80,$9c,$97,$96
		!byte $97,$94,$95,$96,$97,$96,$95,$94
		!byte $95,$00

game_over_sfx	!byte $0b,$00,$02,$a9,$21

		!byte $b9,$b9,$a4,$a4,$b4,$b4,$b0,$b0
		!byte $b0,$b0,$a9,$a9,$b9,$b9,$a4,$a4
		!byte $b4,$b4,$a0,$a0,$a8,$a8,$b0,$b0
		!byte $a9,$a9,$94,$94,$a4,$a4,$a0,$a0
		!byte $a0,$a0,$99,$99,$a9,$a9,$94,$94
		!byte $a4,$a4,$90,$90,$a8,$a8,$b0,$b0
		!byte $99,$99,$84,$84,$94,$94,$90,$90
		!byte $90,$90,$89,$89,$99,$99,$84,$84
		!byte $94,$94,$80,$80,$a8,$a8,$b0,$b0

		!byte $b9,$b9,$a4,$a4,$b4,$b4,$b0,$b0
		!byte $b0,$b0,$a9,$a9,$b9,$b9,$a4,$a4
		!byte $b4,$b4,$a0,$a0,$a8,$a8,$b0,$b0
		!byte $a9,$a9,$94,$94,$a4,$a4,$a0,$a0
		!byte $a0,$a0,$99,$99,$a9,$a9,$94,$94
		!byte $a4,$a4,$90,$90,$a8,$a8,$b0,$b0
		!byte $99,$99,$84,$84,$94,$94,$90,$90
		!byte $90,$90,$89,$89,$99,$99,$84,$84
		!byte $94,$94,$80,$80,$a8,$a8,$b0,$b0

		!byte $00

completion_sfx	!byte $0c,$00,$06,$b0,$41

		!byte $a0,$a0,$a0,$a0,$b0,$b0,$b0,$b0
		!byte $a4,$a4,$a4,$a4,$b4,$b4,$b4,$b4
		!byte $a9,$a9,$a9,$a9,$b9,$b9,$b9,$b9
		!byte $a4,$a4,$a4,$a4,$b4,$b4,$b4,$b4
		!byte $b2,$b2,$b2,$b2,$b2,$b2,$b2,$b2
		!byte $b6,$b6,$b6,$b6,$b6,$b6,$b6,$b6
		!byte $bb,$bb,$bb,$bb,$bb,$bb,$bb,$bb
		!byte $b6,$b6,$b6,$b6,$b6,$b6,$b6,$b6

		!byte $b4,$b4,$b4,$b4,$b4,$b4,$b4,$b4
		!byte $b8,$b8,$b8,$b8,$b8,$b8,$b8,$b8
		!byte $bd,$bd,$bd,$bd,$bd,$bd,$bd,$bd
		!byte $b8,$b8,$b8,$b8,$b8,$b8,$b8,$b8
		!byte $b6,$b6,$b6,$b6,$b6,$b6,$b6,$b6
		!byte $ba,$ba,$ba,$ba,$ba,$ba,$ba,$ba
		!byte $bf,$bf,$bf,$bf,$bf,$bf,$bf,$bf
		!byte $ba,$ba,$ba,$ba,$ba,$ba,$ba,$ba

		!byte $00


; Background tiles data
tile_data	!binary "binary/background.til"

; Background map data
level_data	!binary "binary/background.map"
		!byte $ff,$00,$00,$00,$00	; end of data marker
