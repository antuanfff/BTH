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
; AFX
    include "include\ayFX-ROM.ASM"
; GFX
    ;include "include\metatiles.asm"
; entities
    include "include\entities.asm"
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
	
	;CALL CHGET
	; Empieza el juego    
	call ClearVram_MSX2		
	call SET_SCREEN5_MODE    
    call Set212Lines
    ld	a, BTH_DATA			; page 
	ld	(_bank2),a
        
    call INIT_CHARS_VARS
    call initVDPBuffers    

    LD A, -MOV_SPEED_GHOST
	LD (CHAR_SPEED_X_GHOST), A
    LD HL, PaletteData
    CALL SetPalette
    ; init sfx
    di	
	ld		hl,SONG-99		; hl vale la direccion donde se encuentra la cancion - 99
    PUSH IX
    call	PT3_INIT			; Inicia el reproductor de PT3
	ld hl, AFX
    call ayFX_SETUP
    POP IX
    ei
        
    ; Start STG1    
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
    LD (current_level), A
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
    
    ld	a, BTH_DATA			; page 
	ld	(_bank2),a
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
    
    ; set energy
    LD HL, ANDY_MAX_ENERGY
    ;ADD HL, current_level
    LD A, (HL)  ; no offset for level 1
    ;LD A, 0
    LD (ENTITY_PLAYER_POINTER+3), A
    CALL DRAW_ANDY_ENERGY

    LD A, (stg1_puzzle_solved)
    CP 3
    JR NZ, .nobackfromstg2
        ; Open the gate!
    LD IY, tileDat
    LD (IY + VDP_SX), 96      ; SXL - Tile 2
    LD (IY+VDP_SY), 0      ; SYL
    LD (IY + VDP_DX), 112     ; DXL    
    LD (IY + VDP_DY), 0      ; DYL    
    LD HL, tileDat
    CALL VDPCMD

    ; Modify MAP
    LD HL,stg1_gate
    LD DE, MAP_RAM+14
    LD BC, 4
    LDIR

    LD HL,stg1_gate
    LD DE, MAP_RAM+46
    LD BC, 4
    LDIR
    
.nobackfromstg2
        ; Copy the energy bar to back buffer
    LD HL, DiagBoxToBackBufROM
	call VDPCMD
;	call VDP_Ready
    CALL ENASCR    
    
    ; Testing transparency
    LD IY, tileDatTrans
    LD (IY + VDP_SX), 224      ; SXL - Tile 2
    LD (IY+VDP_SY), 0      ; SYL
    LD (IY + VDP_DX), 0     ; DXL    
    LD (IY + VDP_DY), 0      ; DYL    
    LD HL, tileDatTrans
    CALL VDPCMD
  

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
    JP NZ, .check_john_tomb    
    
    LD A, (stg1_puzzle_solved)
    CP 2
    JP NZ, .puzzle_wrong_order
    INC A
    LD (stg1_puzzle_solved), A
    ; Open the gate!
    LD IY, tileDat
    LD (IY + VDP_SX), 96      ; SXL - Tile 2
    LD (IY+VDP_SY), 0      ; SYL
    LD (IY + VDP_DX), 112     ; DXL    
    LD (IY + VDP_DY), 0      ; DYL    
    LD HL, tileDat
    CALL VDPCMD

    LD IY, stg1_puzzle_solved_strings
    CALL print_strings_dialog_box

    ; Modify MAP
    LD HL,stg1_gate
    LD DE, MAP_RAM+14
    LD BC, 4
    LDIR

    LD HL,stg1_gate
    LD DE, MAP_RAM+46
    LD BC, 4
    LDIR
    ; afx
    LD A,2
    LD C, 0
    CALL ayFX_INIT

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
    ;LD IY, tileDat
    ;LD (IY + VDP_SX), 0      ; SXL - Tile 2
    ;LD (IY+VDP_SY), 0      ; SYL
    ;LD (IY + VDP_DX), 112     ; DXL    
    ;LD (IY + VDP_DY), 0      ; DYL    
    ;LD HL, tileDat
    ;CALL VDPCMD

    LD HL,stg1_gate_blocked
    LD DE, MAP_RAM+45
    LD BC, 6
    LDIR

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
    ; Half open gate
    LD IY, tileDat
    LD (IY + VDP_SX), 64      ; SXL - Tile 2
    LD (IY+VDP_SY), 0      ; SYL
    LD (IY + VDP_DX), 112     ; DXL    
    LD (IY + VDP_DY), 0      ; DYL    
    LD HL, tileDat
    CALL VDPCMD
    ; afx
    LD A,0
    LD C, 0
    CALL ayFX_INIT

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
    ; Remove the lockpad
    LD IY, tileDat
    LD (IY + VDP_SX), 32      ; SXL - Tile 1
    LD (IY+VDP_SY), 0      ; SYL
    LD (IY + VDP_DX), 112     ; DXL    
    LD (IY + VDP_DY), 0      ; DYL    
    LD HL, tileDat
    CALL VDPCMD

    LD HL,stg1_gate
    LD DE, MAP_RAM+47
    LD BC, 2
    LDIR

    ; afx
    LD A,1
    LD C, 0
    CALL ayFX_INIT

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
    LD A, (ix+SPR_GHOST_STG1+1)     ; Cargamos la X
    LD (ENTITY_ENEMY1_POINTER+1), A
    LD A, (ix+SPR_GHOST_STG1)           ; Cargamos la Y
    LD (ENTITY_ENEMY1_POINTER+2), A
    
    LD IY, ENTITY_ENEMY1_POINTER
    CALL EnemyCollisionCheck
    JR NC, .move_shoot
    ; Collision
    LD A, (ENTITY_PLAYER_POINTER+ENTITY_ENERGY)
    SUB 4    
    LD (ENTITY_PLAYER_POINTER+ENTITY_ENERGY), A
    call DRAW_ANDY_ENERGY
    call BOUNCE_ANDY
    LD A, (ENTITY_PLAYER_POINTER+ENTITY_ENERGY)
    CP 0
    JP Z, game_over

.move_shoot:
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
	call	PT3_ROUT			;envia datos a al PSG 	   
	call	PT3_PLAY			;prepara el siguiente trocito de cancion que sera enviada mas tarde al PSG
	call ayFX_PLAY
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


STAGE2:
    CALL DISSCR
    halt
	di       
    PUSH IX
	call	PT3_ROUT			;envia datos a al PSG 	   
	call	PT3_PLAY			;prepara el siguiente trocito de cancion que sera enviada mas tarde al PSG
	POP IX
    ei
    LD HL, CEMENTER2
    LD (BITMAP), HL
    LD B, :CEMENTER2

    call load_screen
    LD HL, mapa2
    LD (MAPA), HL
    
    ; Ponemos el P1 por encima del marco
    LD (ix), 175      ; mask 0
    LD (ix+4), 175    ; mask 1
    LD (ix+8), 175    ; mask 2
    
    LD (ix+SPR_GHOST_STG1),217  ; ocultamos el fantasma
    LD (ix+SPR_GHOST_STG1+4),217  ; ocultamos el fantasma
    
    CALL DUMP_SPR_ALL
    CALL DUMP_SPR_P1        
    
    ;LD HL, ANDY_MAX_ENERGY
    ;LD A, (HL)  
    ;INC A           ; level 1
    ;LD (ENTITY_PLAYER_POINTER+3), A

    ;XOR a
    ;LD (current_level), A
    CALL DRAW_ANDY_ENERGY
    
    CALL ENASCR
    
MAIN_LOOP2:
    ;halt    
    halt
	di       
    PUSH IX
	call	PT3_ROUT			;envia datos a al PSG 	   
	call	PT3_PLAY			;prepara el siguiente trocito de cancion que sera enviada mas tarde al PSG
	call ayFX_PLAY
    POP IX
    ei
    LD A, (ix)    
    CP 176      ; Miramos si la Y es 160 para pasar a stg1
    JP NZ, .no_screen_change
    ; Ponemos el P1 al principio de la pantalla 1
    LD (ix), 1          ; P1.Y = 1
    LD (ix+4), 1
    LD (ix+8), 1

    LD A, (CHAR_GHOST_DEAD)
    CP $01
    JP Z, .GHOST_DEAD
    ld (ix+SPR_GHOST_STG1), $0F      ; Sprite 1 - Ghost
    ld (ix+SPR_GHOST_STG1+4), $0F      ; Sprite 1 - Ghost
.GHOST_DEAD:    
    CALL STAGE1

.no_screen_change:
    
    ; check X,Y to play Black Sabbath
    ; Ya tenemos en A la Y
    CP STG2_TILE1_Y
    JP NZ, .check_tile3
    LD A, (ix+1)
    CP STG2_TILE1_X
    JP NZ, .check_next_tile
    LD A, 3
    LD C, 0
    CALL ayFX_INIT    

    LD IY, tileDat
    LD (IY + VDP_SX), 192      ; SXL - Tile 2
    LD (IY+VDP_SY), 0      ; SYL
    LD (IY+VDP_NX), 16      ; NX    
    LD (IY + VDP_DX), 112     ; DXL    
    LD (IY + VDP_DY), 112      ; DYL    
    LD HL, tileDat
    CALL VDPCMD
    jr .continue

.check_next_tile:
    CP STG2_TILE2_X
    JP NZ, .check_tile3
    LD A, 4
    LD C, 0
    CALL ayFX_INIT    

    LD IY, tileDat
    LD (IY + VDP_SX), 192      ; SXL - Tile 2
    LD (IY+VDP_SY), 0      ; SYL
    LD (IY+VDP_NX), 16      ; NX    
    LD (IY + VDP_DX), 128     ; DXL    
    LD (IY + VDP_DY), 112      ; DYL    
    LD HL, tileDat
    CALL VDPCMD

.check_tile3:
    CP STG2_TILE3_Y
    JP NZ, .continue
    LD A, (ix+1)
    CP STG2_TILE3_X
    JP NZ, .continue
    LD A, 5
    LD C, 0
    CALL ayFX_INIT    

    LD IY, tileDat
    LD (IY + VDP_SX), 192      ; SXL - Tile 2
    LD (IY+VDP_SY), 0      ; SYL
    LD (IY+VDP_NX), 16      ; NX    
    LD (IY + VDP_DX), 128     ; DXL    
    LD (IY + VDP_DY), 144      ; DYL    
    LD HL, tileDat
    CALL VDPCMD

.continue:
    call DUMP_SPR_ATTS      
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



    jp MAIN_LOOP2

game_over:
    LD IY, game_over_strings
    call print_strings_dialog_box
    call CHGET
    call CHGET
    call CHGET
    JP START

AFX:
    incbin "sfx\cementer_sounds.afb"
SONG:
	;incbin "sfx\Nostalgy_sincabecera.pt3"
    incbin "sfx\test.pt3"
    ;incbin "sfx\G-6sin_cabecera.pt3"

TILES1:
 INCBIN "gfx\tiles1.sc5",#7

 PAGE 1
; CODE O NO
    include "include\BTH_data.asm"
FONT:
 INCBIN "gfx\FONT.SC5",#7

 PAGE 2

 PAGE 3

 PAGE 4
 PAGE 5
 PAGE 6
 PAGE 7
CEMENTER1
 ;#Para el fondo borrar de BDA0 en adelante para quitar la parte de la energia
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
;#Para el fondo borrar de BDA0 en adelante para quitar la parte de la energia
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
    include "include\ayFX-RAM.ASM"
	ENDMAP
  