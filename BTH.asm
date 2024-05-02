	output "BTH.rom" 

;ROM ASCII16
		defpage	0,0x4000, 0x4000
		defpage	1,0x8000, 0x4000        
		defpage	2..14 ;Here it is determined rom size        
        defpage	15,0xC000, 0x4000

PageSize:	    equ	0x4000	        ; 16kB
_bank1	equ	6000h
_bank2	equ	7000h
		DB 32h,0,70h,32h,0FFh,77h		; Para evitar seleccionar el tipo de ROM ASCII16 en OpenMSX
		page 0			
        org 4000h		
        dw  4241h,START,0,0,0,0,0,0

;Llamadas a rutinas bios
    include "include\System_BIOSCalls.asm"

;Constantes
    include "include\BTH_const.asm"
    include "include\BTH_strings.asm"
; Funciones auxiliares
	include "include\BTH_func.asm"
    include "include\BTH_animate.asm"
	include "include\VDP.asm"
    include "include\VDP_Data.asm"
; SFX
    include	"include\PT3_player.s"  
START
	; CODE
    ld hl,FORCLR ; Variable del Sistema
	ld [hl],0 ; Color del primer plano 15=blanco
	inc hl ; FORCLR+1
	ld [hl],0 ; Color de fondo 1=negro
	inc hl ; FORCLR+2
	ld [hl],0 ; Color del borde 1=negro
	LD A,8
	;CALL CHGMOD    	
    CALL SETPAGES32K
	;CALL opening_screen
	LD A,1
	LD (_bank2),A
	;CALL CHGET
	; Empieza el juego    
	call ClearVram_MSX2		
	call SET_SCREEN5_MODE    
    call Set212Lines
        
    call INIT_CHARS_VARS
    call initVDPBuffers

    LD A, -MOV_SPEED_GHOST
	LD (CHAR_SPEED_X_GHOST), A
    LD HL, PaletteData
    CALL SetPalette
    CALL STAGE1
    ;call MAIN_LOOP
    ;CALL CHGET
	ret


INIT_CHARS_VARS:    
    ld ix, SPRITE_ATTRS           
    
    ld (ix), 69h        ; Sprite 1 - $AF abajo - Y
    ld (ix+1), 7Fh      ; X        
    ld (ix+2), 00h      ; Pattern

    ld (ix+4), 69h     ; Sprite 2
    ld (ix+5), 7Fh
    ld (ix+6), 04h        

    ld (ix+8), 69h     ; Sprite 3
    ld (ix+9), 7Fh
    ld (ix+10), 08h        

    ld (ix+SPR_GHOST_STG1), $0f      ; Sprite 1 - Ghost - mask0
    ld (ix+SPR_GHOST_STG1+1), $AF
    ld (ix+SPR_GHOST_STG1+2), SPR_GHOST_STG1_PTRN_L1
    
    ld (ix+SPR_GHOST_STG1+4), $0f      ; Sprite 1 - Ghost - mask0
    ld (ix+SPR_GHOST_STG1+5), $AF
    ld (ix+SPR_GHOST_STG1+6), SPR_GHOST_STG1_PTRN_L1+4
    
    XOR A
    LD (JIFFY_TEMP),A
    LD (CHAR_SPEED_X),A
    LD (CHAR_SPEED_Y),A
    LD (CHAR_SPEED_X_GHOST),A
    LD (CHAR_DIR_GHOST1),A      ; $00 - LEFT, $FF - RIGHT
    LD (CHAR_NEW_DIR_MAIN),A    
    LD (CHAR_MAIN_SHOOT),A      ; Indica si el personaje está disparando - $FF
    LD (CHAR_DISTANCE_SHOOT),A
    LD (CHAR_SPEED_SHOOT),A
    LD (CHAR_GHOST_DEAD),A
    LD (CHAR_MIN_STEP), A
    LD (SPRITE_COLOR_REPLACE2), A
    LD (SHOWING_GUS_DIALOG), A
    LD (SHOWING_JOHN_DIALOG), A
    LD (SHOWING_MIKE_DIALOG), A
    LD (SHOWING_SKULL_STG1_DIALOG), A    
    LD (stg1_puzzle_solved), A
   ; LD A,$FF
    LD (OLD_KEY_PRESSED), A
    LD A,$01
    LD (CHAR_DIR_MAIN),A        ; $00 - UP, $01 - DOWN, $02 - LEFT, $03 - RIGHT

    ld hl, SPRITE_P1_DOWN
    ld (SPRITE_PTR_REPLACE), HL    
    ld hl, SPRITE_COLOR_P1_DOWN
    ld (SPRITE_COLOR_REPLACE), HL
    LD (SPRITE_COLOR_REPLACE2), HL

    
    ret

STAGE1:
    CALL DISSCR
    LD HL, CEMENTER1
    LD (BITMAP), HL
    LD B, :CEMENTER1
    call load_screen
    
    call DUMP_SPR_ALL
    CALL DUMP_SPR_P1
    
    LD HL, mapa1
    LD DE, MAP_RAM
    LD BC, 736
    LDIR
    
    LD HL, MAP_RAM
    LD (MAPA), HL
    
    ;We load the tiles on page 1 of VDP
    LD HL, TILES1    
    call load_tiles_vdp

    LD A, (stg1_puzzle_solved)
    CP 3
    JR NZ, .backfromstg2
        ; Open the gate!
    LD IY, tileDat
    LD (IY + VDP_SX), 64      ; SXL - Tile 2
    LD (IY+VDP_SY), 0      ; SYL
    LD (IY + VDP_DX), 112     ; DXL    
    LD (IY + VDP_DY), 0      ; DYL    
    LD HL, tileDat
    CALL VDPCMD

.backfromstg2
    CALL ENASCR    

   	di	
	ld		hl,SONG-99		; hl vale la direccion donde se encuentra la cancion - 99
    PUSH IX
    call	PT3_INIT			; Inicia el reproductor de PT3
	POP IX
    ei


MAIN_LOOP:
    ;halt ; sincroniza el teclado y pantalla con el procesador (que va muy rápido)    

    
    LD A, (ix)  ; Cargamos la Y
    CP $00
    JP Z, STAGE2
    call DUMP_SPR_ATTS    

.check_tombs:

    LD A, (stg1_puzzle_solved)
    CP 3
    JP Z, .animate_ghost

    LD A, (ix +1)   ; Cargamos la X para mirar si hay colisión con la tumba
    CP MIKE_TOMB_STG1_X
    JR NZ, .check_john_tomb    
    
    LD A, (stg1_puzzle_solved)
    CP 2
    JP NZ, .puzzle_wrong_order
    INC A
    LD (stg1_puzzle_solved), A
    ; Open the gate!
    LD IY, tileDat
    LD (IY + VDP_SX), 64      ; SXL - Tile 2
    LD (IY+VDP_SY), 0      ; SYL
    LD (IY + VDP_DX), 112     ; DXL    
    LD (IY + VDP_DY), 0      ; DYL    
    LD HL, tileDat
    CALL VDPCMD

    LD IY, stg1_puzzle_solved_strings
    CALL print_strings_dialog_box
    JP .animate_ghost

.puzzle_wrong_order
    LD A, (SHOWING_MIKE_DIALOG)
    CP 1
    JP Z, .animate_ghost    
    LD IY, mike_tomb_strings
    CALL print_strings_dialog_box
    LD A,1
    LD (SHOWING_MIKE_DIALOG), A
    XOR A
    LD (stg1_puzzle_solved), A
            ; Close the gate!
    LD IY, tileDat
    LD (IY + VDP_SX), 0      ; SXL - Tile 2
    LD (IY+VDP_SY), 0      ; SYL
    LD (IY + VDP_DX), 112     ; DXL    
    LD (IY + VDP_DY), 0      ; DYL    
    LD HL, tileDat
    CALL VDPCMD

    JP .animate_ghost

.check_john_tomb:
    CP JOHN_TOMB_STG1_X
    jr nz, .check_gus_tomb
    LD A, (SHOWING_JOHN_DIALOG)
    CP 1
    JP Z, .animate_ghost
    LD IY, john_tomb_strings
    CALL print_strings_dialog_box
    LD A,1
    LD (SHOWING_JOHN_DIALOG), A
    LD A, (stg1_puzzle_solved)
    CP 1
    JP NZ, .animate_ghost
    INC A
    LD (stg1_puzzle_solved), A
    JP .animate_ghost

.check_gus_tomb:
    CP GUS_TOMB_STG1_X
    JP nz, .check_skull_hint
    LD A, (SHOWING_GUS_DIALOG)
    CP 1
    JP Z, .animate_ghost
    LD A, (ix)
    CP GUS_TOMB_STG1_Y
    jp c, .animate_ghost
    LD IY, gus_tomb_strings
    CALL print_strings_dialog_box
    LD A,1
    LD (SHOWING_GUS_DIALOG), A
    LD (stg1_puzzle_solved), A
    ; Open the gate (half)!
    LD IY, tileDat
    LD (IY + VDP_SX), 32      ; SXL - Tile 1
    LD (IY+VDP_SY), 0      ; SYL
    LD (IY + VDP_DX), 112     ; DXL    
    LD (IY + VDP_DY), 0      ; DYL    
    LD HL, tileDat
    CALL VDPCMD
    JR .animate_ghost

.check_skull_hint:
    CP SKULL_TOMB_STG1_X
    jr nz, .check_mike_dialog_box
        
    LD A, (ix)
    CP SKULL_TOMB_STG1_Y1
    jr c, .check_mike_dialog_box
    CP SKULL_TOMB_STG1_Y2
    jr nc, .check_mike_dialog_box

    LD A, (SHOWING_SKULL_STG1_DIALOG)
    CP 1
    JR Z, .animate_ghost
    LD IY, stg1_skull_strings
    CALL print_strings_dialog_box
    LD A,1
    LD (SHOWING_SKULL_STG1_DIALOG), A
    JR .animate_ghost

.check_mike_dialog_box
    LD A, (SHOWING_MIKE_DIALOG)
    CP 1
    JR nz, .check_gus_dialog
    CALL CLEAR_DIALOG_BOX
    XOR A
    LD (SHOWING_MIKE_DIALOG), A
    JR .animate_ghost

.check_gus_dialog
    LD A, (SHOWING_GUS_DIALOG)
    CP 1
    JR nz, .check_john_dialog
    CALL CLEAR_DIALOG_BOX
    XOR A
    LD (SHOWING_GUS_DIALOG), A
    JR .animate_ghost

.check_john_dialog
    LD A, (SHOWING_JOHN_DIALOG)
    CP 1
    JR nz, .check_skull_dialog
    CALL CLEAR_DIALOG_BOX
    XOR A
    LD (SHOWING_JOHN_DIALOG), A
    JR .animate_ghost

.check_skull_dialog
    LD A, (SHOWING_SKULL_STG1_DIALOG)
    CP 1
    JR nz, .animate_ghost
    CALL CLEAR_DIALOG_BOX
    XOR A
    LD (SHOWING_SKULL_STG1_DIALOG), A

.animate_ghost    
    LD A,(CHAR_GHOST_DEAD)
    CP $01
    JP Z,.continue
    LD A, (ix+SPR_GHOST_STG1+1)          ;cargamos la X del Fantasma
	LD HL, (CHAR_SPEED_X_GHOST)
	ADD L					; Actualizamos la posicion en base a la velocidad
    
	LD (ix+SPR_GHOST_STG1+1), A
    LD (ix+SPR_GHOST_STG1+5), A
    CP $50
    JP Z,.CHANGE_DIR_RIGHT
    CP $AF
    JP Z,.CHANGE_DIR_LEFT
    		
    JP .check_pattern

.CHANGE_DIR_RIGHT:
    LD A, MOV_SPEED_GHOST
	LD (CHAR_SPEED_X_GHOST), A
    LD A,$FF
    LD (CHAR_DIR_GHOST1),A
    JP .check_pattern

.CHANGE_DIR_LEFT:
    LD A, -MOV_SPEED_GHOST
	LD (CHAR_SPEED_X_GHOST), A
    XOR A   ; Pone A a 0
    LD (CHAR_DIR_GHOST1),A

.check_pattern:
    LD A,(CHAR_DIR_GHOST1)
    CP $FF
    JP Z,.check_pattern_RIGHT
    LD A, (ix+SPR_GHOST_STG1+2)       ; Cargamos el patrón y lo cambiamos
    CP SPR_GHOST_STG1_PTRN_L1
    jp z,.change_pattern_L
    LD (ix+SPR_GHOST_STG1+2),SPR_GHOST_STG1_PTRN_L1
    LD (ix+SPR_GHOST_STG1+6),SPR_GHOST_STG1_PTRN_L1+4
    jp .continue
.change_pattern_L:
    LD (ix+SPR_GHOST_STG1+2),SPR_GHOST_STG1_PTRN_L2
    LD (ix+SPR_GHOST_STG1+6),SPR_GHOST_STG1_PTRN_L2+4
    jp .continue

.check_pattern_RIGHT
    LD A, (ix+SPR_GHOST_STG1+2)       ; Cargamos el patrón y lo cambiamos
    CP SPR_GHOST_STG1_PTRN_R1
    jp z,.change_pattern_R
    LD (ix+SPR_GHOST_STG1+2),SPR_GHOST_STG1_PTRN_R1
    LD (ix+SPR_GHOST_STG1+6),SPR_GHOST_STG1_PTRN_R1+4
    jp .continue
.change_pattern_R:
    LD (ix+SPR_GHOST_STG1+2),SPR_GHOST_STG1_PTRN_R2
    LD (ix+SPR_GHOST_STG1+6),SPR_GHOST_STG1_PTRN_R2+4

.continue:    
    CALL MOVE_SHOOT
.CHECK_GHOST:
    ; Comprobamos si hay colision con el fantasma
    LD A,(CHAR_GHOST_DEAD)  ; si está muerto,no lo miramos
    CP $01
    JP Z,.check_KB

    LD B,(ix+SPR_SHOOT_P1)         ; Y del disparo
    LD C,(ix+SPR_SHOOT_P1+1)         ; X del disparo
    LD D,(ix+SPR_GHOST_STG1)         ; Y del fantasma 
    LD E,(ix+SPR_GHOST_STG1+1)         ; X del fantasma
    call check_spr_collision
    CP 1
    JP NZ,.check_KB

.KILL_GHOST:
    LD A,1
    LD (CHAR_GHOST_DEAD),A
    LD (ix+SPR_GHOST_STG1),217    
    LD (ix+SPR_GHOST_STG1+4),217    

.check_KB:
    halt    
	
	di       
    PUSH IX
	;call	PT3_ROUT			;envia datos a al PSG 	   
	;call	PT3_PLAY			;prepara el siguiente trocito de cancion que sera enviada mas tarde al PSG
	POP IX
    ei

    ld a, 8
	call SNSMAT   
    
    LD C,A    
    
    BIT KB_RIGHT, C			; La tecla presionada es RIGHT?
    call z, move_right
    
    BIT KB_LEFT, C			; La tecla presionada es LEFT?
    call z, move_left

    BIT KB_UP, C			; La tecla presionada es UP?
    call z, move_up

    BIT KB_DOWN, C			; La tecla presionada es DOWN?
    call z, move_down

no_arrows:
    BIT KB_SPACE, C			; La tecla presionada es SPACE
    call z,SHOOT_MAIN_CHAR

    ;BIT KB_DEL, C			; La tecla presionada es DEL    
    ;ret z
    
    jp MAIN_LOOP

SHOOT_MAIN_CHAR:    
    LD A, (CHAR_MAIN_SHOOT)
    CP $01                  ; Si ya está disparando esperamos a que termine
    RET Z;,MAIN_LOOP
    CP $02                  ; Si ya está disparando esperamos a que termine
    RET Z;,MAIN_LOOP
    CP $03                  ; Si ya está disparando esperamos a que termine
    RET Z;,MAIN_LOOP
    CP $04                  ; Si ya está disparando esperamos a que termine
    RET Z;,MAIN_LOOP

    ld (ix+SPR_SHOOT_P1+2), SPR_SHOOT_P1_PTRN     ; Sprite Disparo

    LD A, (ix)          
    ld (ix+SPR_SHOOT_P1), A       ; Asignamos la Y del personaje    

    LD A, (CHAR_DIR_MAIN)
    CP $03
    JP Z,.SHOOT_RIGHT
    CP $00
    JP Z,.SHOOT_UP
    CP $01
    JP Z,.SHOOT_DOWN
    ; SHOOT LEFT
    LD A,$01                ; SHOOT LEFT
    LD (CHAR_MAIN_SHOOT),A   ; Activo el estado disparando izquierda
    LD A, (ix+1)			;cargamos la X - Si no es derecha, debe ser izquierda
	LD HL, -12
	ADD L
    
    JP .CONTINUE

.SHOOT_RIGHT:    
    LD A,$02
    LD (CHAR_MAIN_SHOOT),A   ; Activo el estado disparando derecha
    
    LD A, (ix+1)			;cargamos la X
	LD HL, 12
	ADD L
    JP .CONTINUE

.SHOOT_UP:
    LD A,$03
    LD (CHAR_MAIN_SHOOT),A   ; Activo el estado disparando arriba
    
    ; Sumamos el desplazamiento a la Y
    LD A, (ix+SPR_SHOOT_P1)
    SUB 16
    LD (ix+SPR_SHOOT_P1), A    
    ;LD (ix+17), D
    LD A, (ix+1)			;cargamos la X    
    JP .CONTINUE

.SHOOT_DOWN
	LD A,$04
    LD (CHAR_MAIN_SHOOT),A   ; Activo el estado disparando abajo
    
    ; Sumamos el desplazamiento a la Y
    LD A, (ix+SPR_SHOOT_P1)
    ADD 16
    LD (ix+SPR_SHOOT_P1), A    
    ;LD (ix+17), D
    LD A, (ix+1)			;cargamos la X    

.CONTINUE:
    ;ld (ix+16), B       ; Asignamos la Y del personaje
    ld (ix+SPR_SHOOT_P1+1), A       ; Asignamos la X del personaje + el desplazamiento        
    ;jp MAIN_LOOP
    ret

STAGE2:
    CALL DISSCR
    LD HL, CEMENTER2
    LD (BITMAP), HL
    LD B, :CEMENTER2

    call load_screen
    LD HL, mapa2
    LD (MAPA), HL
    
    ; Ponemos el P1 por encima del marco
    LD (ix), 161      ; mask 0
    LD (ix+4), 161    ; mask 1
    LD (ix+8), 161    ; mask 2
    
    LD (ix+SPR_GHOST_STG1),217  ; ocultamos el fantasma
    LD (ix+SPR_GHOST_STG1+4),217  ; ocultamos el fantasma
    
    CALL DUMP_SPR_ALL
    CALL DUMP_SPR_P1
    
    ; Esqueleto
    LD (ix+SPR_GHOST_STG2), 10h
    LD (ix+SPR_GHOST_STG2+1), 10h
    LD (ix+SPR_GHOST_STG2+2), SPR_GHOST_STG2_PTRN_L1

    LD (ix+SPR_GHOST_STG2+4), 10h
    LD (ix+SPR_GHOST_STG2+5), 10h
    LD (ix+SPR_GHOST_STG2+6), SPR_GHOST_STG2_PTRN_L1+4

    XOR A
    LD (CHAR_GHOST_DEAD_STG2), A
    LD (CHAR_MIN_STEP_STG2), A
    LD A, $FF
    LD (CHAR_DIR_GHOST_STG2), A
    LD A, MOV_SPEED_GHOST
	LD (CHAR_SPEED_X_GHOST_STG2), A

    CALL ENASCR
    
MAIN_LOOP2:
    ;halt    
   
    LD A, (ix)    
    CP 162      ; Miramos si la Y es 160 para pasar a stg1
    JP NZ, .no_screen_change
    ; Ponemos el SP1 al principio de la pantalla 1
    LD (ix), 1          ; SP1 - Y = 1
    LD (ix+4), 1
    LD (ix+8), 1

    LD A, (CHAR_GHOST_DEAD)
    CP $01
    JP Z, .GHOST_DEAD
    ld (ix+SPR_GHOST_STG1), $0F      ; Sprite 1 - Ghost
    ld (ix+SPR_GHOST_STG1+4), $0F      ; Sprite 1 - Ghost
.GHOST_DEAD:
    LD (ix+SPR_GHOST_STG2),217  ; ocultamos el esqueleto
    LD (ix+SPR_GHOST_STG2+4),217  ; ocultamos el esqueleto
    CALL STAGE1

.no_screen_change:

    call DUMP_SPR_ATTS    
    ; Movemos el esqueleto
    LD A,(CHAR_GHOST_DEAD_STG2)
    CP $01
    JP Z,.continue
    LD A, (ix+SPR_GHOST_STG2+1)          ;cargamos la X del Esqueleto
	LD HL, (CHAR_SPEED_X_GHOST_STG2)
	ADD L					; Actualizamos la posicion en base a la velocidad
    
	LD (ix+SPR_GHOST_STG2+1), A	
    LD (ix+SPR_GHOST_STG2+5), A
    CP $16
    JP Z,.CHANGE_DIR_RIGHT
    CP $B9
    JP Z,.CHANGE_DIR_LEFT

    LD A, (CHAR_MIN_STEP_STG2)		
    CP MAX_CHAR_STEPS_STG2
    JP Z,.check_pattern
    ADD 1
    LD (CHAR_MIN_STEP_STG2), A
    JP .continue

.CHANGE_DIR_RIGHT:
    LD A, MOV_SPEED_GHOST
	LD (CHAR_SPEED_X_GHOST_STG2), A
    LD A,$FF
    LD (CHAR_DIR_GHOST_STG2),A
    JP .check_pattern

.CHANGE_DIR_LEFT:
    LD A, -MOV_SPEED_GHOST
	LD (CHAR_SPEED_X_GHOST_STG2), A
    XOR A   ; Pone A a 0
    LD (CHAR_DIR_GHOST_STG2),A

.check_pattern:
    XOR A
    LD (CHAR_MIN_STEP_STG2), A
    LD A,(CHAR_DIR_GHOST_STG2)
    CP $FF
    JP Z,.check_pattern_RIGHT
    LD A, (ix+SPR_GHOST_STG2+2)       ; Cargamos el patrón y lo cambiamos
    CP SPR_GHOST_STG2_PTRN_L1
    jp z,.change_pattern_L
    LD (ix+SPR_GHOST_STG2+2),SPR_GHOST_STG2_PTRN_L1
    LD (ix+SPR_GHOST_STG2+6),SPR_GHOST_STG2_PTRN_L1+4
    jp .continue
.change_pattern_L:
    LD (ix+SPR_GHOST_STG2+2),SPR_GHOST_STG2_PTRN_L2
    LD (ix+SPR_GHOST_STG2+6),SPR_GHOST_STG2_PTRN_L2+4
    jp .continue

.check_pattern_RIGHT
    LD A, (ix+SPR_GHOST_STG2+2)       ; Cargamos el patrón y lo cambiamos
    CP SPR_GHOST_STG2_PTRN_R1
    jp z,.change_pattern_R
    LD (ix+SPR_GHOST_STG2+2),SPR_GHOST_STG2_PTRN_R1
    LD (ix+SPR_GHOST_STG2+6),SPR_GHOST_STG2_PTRN_R1+4
    jp .continue
.change_pattern_R:
    LD (ix+SPR_GHOST_STG2+2),SPR_GHOST_STG2_PTRN_R2
    LD (ix+SPR_GHOST_STG2+6),SPR_GHOST_STG2_PTRN_R2+4

.continue:
    CALL MOVE_SHOOT    

    ld a, 8
	call SNSMAT   
    LD C,A    
        
    BIT KB_RIGHT, C			; La tecla presionada es RIGHT?
    call z, move_right
    
    BIT KB_LEFT, C			; La tecla presionada es LEFT?
    call z, move_left

    BIT KB_UP, C			; La tecla presionada es UP?
    call z, move_up

    BIT KB_DOWN, C			; La tecla presionada es DOWN?
    call z, move_down

    BIT KB_SPACE, C			; La tecla presionada es SPACE
    call z,SHOOT_MAIN_CHAR

    BIT KB_DEL, C			; La tecla presionada es DEL    
    ret z

    halt
	;di       
    ;PUSH IX
	;call	PT3_ROUT			;envia datos a al PSG 	   
	;call	PT3_PLAY			;prepara el siguiente trocito de cancion que sera enviada mas tarde al PSG
	;POP IX
    ;ei

    jp MAIN_LOOP2


SONG:
	;incbin "musica_sin_cabacera.pt3"
    incbin "sfx\test.pt3"
    ;incbin "sfx\G-6sin_cabecera.pt3"
include "include\BTH_data.asm"
TILES1:
 INCBIN "gfx\tiles1.sc5",#7

 PAGE 1
; CODE O NO

 PAGE 2

 PAGE 3

 PAGE 4
 PAGE 5
 PAGE 6
FONT:
 INCBIN "gfx\FONT.SC5",#7
 PAGE 7
CEMENTER1
 INCBIN "gfx\CEMENTER1.SC5",#7,#4000			; Cada página tiene 16K = 4000h
 PAGE 8
 INCBIN "gfx\CEMENTER1.SC5",#4007			; Cada página tiene 16K = 4000h 
 PAGE 9
GRAPHIC
 INCBIN "gfx\BTH.SR8",#7,#4000			; Cada página tiene 16K = 4000h
 PAGE 10
 INCBIN "gfx\BTH.SR8",#4007,#4000

 PAGE 11
 INCBIN "gfx\BTH.SR8",#8007,#4000

 PAGE 12
 INCBIN "gfx\BTH.SR8",#C007

 PAGE 13
CEMENTER2
 INCBIN "gfx\CEMENTER2.SC5",#7,#4000			; Cada página tiene 16K = 4000h
 PAGE 14
 INCBIN "gfx\CEMENTER2.SC5",#4007			; Cada página tiene 16K = 4000h 
 PAGE 15
;---------------------------------------------------------
; Variables
;---------------------------------------------------------
	
	MAP #C000  ;Ram page 3
;	MAP #E5FF  ;Ram page 3
;NAME #1  ;a byte is reserved for this variable
;ETC
;
	include "include\BTH_RAM.asm"
	ENDMAP
  