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

; Funciones auxiliares
	include "include\BTH_func.asm"
    include "include\BTH_animate.asm"
	;include "include\SETPAGES48K.asm"

START
	; CODE
	;LD A,8
	;CALL CHGMOD    	
    CALL SETPAGES32K
;	CALL opening_screen
	LD A,1
	LD (_bank2),A
;	CALL CHGET
	; Empieza el juego    
	call ClearVram_MSX2		
	call SET_SCREEN5_MODE    
    call Set212Lines
        
    call INIT_CHARS_VARS
    LD A, -MOV_SPEED_GHOST
	LD (CHAR_SPEED_X_GHOST), A
    CALL STAGE1
    ;call MAIN_LOOP
    ;CALL CHGET
	ret


INIT_CHARS_VARS:    
    ld ix, SPRITE_ATTRS           
    
    ld (ix), 07h        ; Sprite 1 - $AF abajo
    ld (ix+1), 09h
    ld (ix+2), 00h    

    ld (ix+4), 07h     ; Sprite 2
    ld (ix+5), 09h
    ld (ix+6), 04h        

    ld (ix+8), 07h     ; Sprite 3
    ld (ix+9), 09h
    ld (ix+10), 08h        

    ld (ix+12), $07      ; Sprite 1 - Ghost
    ld (ix+13), $B9
    ld (ix+14), $18
    
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
    LD (MAPA), HL    
    CALL ENASCR

MAIN_LOOP:
    ;halt ; sincroniza el teclado y pantalla con el procesador (que va muy rápido)    
    LD A, (ix)  ; Cargamos la Y
    CP $00
    JP Z, STAGE2
    call DUMP_SPR_ATTS    
    
    LD A,(CHAR_GHOST_DEAD)
    CP $01
    JP Z,.continue
    LD A, (ix+13)          ;cargamos la X del Fantasma
	LD HL, (CHAR_SPEED_X_GHOST)
	ADD L					; Actualizamos la posicion en base a la velocidad
    
	LD (ix+13), A	
    CP $16
    JP Z,.CHANGE_DIR_RIGHT
    CP $B9
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
    LD A, (ix+14)       ; Cargamos el patrón y lo cambiamos
    CP $18
    jp z,.change_pattern_L
    LD (ix+14),$18
    jp .continue
.change_pattern_L:
    LD (ix+14),$1C
    jp .continue

.check_pattern_RIGHT
    LD A, (ix+14)       ; Cargamos el patrón y lo cambiamos
    CP $20
    jp z,.change_pattern_R
    LD (ix+14),$20
    jp .continue
.change_pattern_R:
    LD (ix+14),$24

.continue:    
    CALL MOVE_SHOOT
.CHECK_GHOST:
    ; Comprobamos si hay colision con el fantasma
    LD A,(CHAR_GHOST_DEAD)  ; si está muerto,no lo miramos
    CP $01
    JP Z,.check_KB

    LD B,(ix+16)         ; Y del disparo
    LD C,(ix+17)         ; X del disparo
    LD D,(ix+12)         ; Y del fantasma 
    LD E,(ix+13)         ; X del fantasma
    call check_spr_collision
    CP 1
    JP NZ,.check_KB

.KILL_GHOST:
    LD A,1
    LD (CHAR_GHOST_DEAD),A
    LD (ix+12),217    

.check_KB:
    halt    

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

    BIT KB_DEL, C			; La tecla presionada es DEL    
    ret z

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

    ld (ix+18), $28     ; Sprite Disparo

    LD A, (ix)          
    ld (ix+16), A       ; Asignamos la Y del personaje    

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
    LD (CHAR_MAIN_SHOOT),A   ; Activo el estado disparando derecha
    
    ; Sumamos el desplazamiento a la Y
    LD A, (ix+16)
    SUB 16
    LD (ix+16), A    
    ;LD (ix+17), D
    LD A, (ix+1)			;cargamos la X    
    JP .CONTINUE

.SHOOT_DOWN
	LD A,$04
    LD (CHAR_MAIN_SHOOT),A   ; Activo el estado disparando derecha
    
    ; Sumamos el desplazamiento a la Y
    LD A, (ix+16)
    ADD 16
    LD (ix+16), A    
    ;LD (ix+17), D
    LD A, (ix+1)			;cargamos la X    

.CONTINUE:
    ;ld (ix+16), B       ; Asignamos la Y del personaje
    ld (ix+17), A       ; Asignamos la X del personaje + el desplazamiento        
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

    LD (ix), 196    ; Ponemos el P1 abajo
    LD (ix+4), 196    
    LD (ix+8), 196    
    
    LD (ix+12),217  ; ocultamos el fantasma
    
    call DUMP_SPR_ALL    
    CALL DUMP_SPR_P1
    ; Esqueleto
    LD (ix+20), 10h
    LD (ix+21), 10h
    LD (ix+22), 2Ch

    LD (ix+24), 10h
    LD (ix+25), 10h
    LD (ix+26), 30h

    CALL ENASCR
    
MAIN_LOOP2:
    halt    

    LD A, (ix)    
    CP 198
    JP NZ, .no_screen_change
    LD (ix), 1          ; SP1 - Y = 1
    LD (ix+4), 1
    LD (ix+8), 1

    LD A, (CHAR_GHOST_DEAD)
    CP $01
    JP Z, .GHOST_DEAD
    ld (ix+12), $07      ; Sprite 1 - Ghost
.GHOST_DEAD:
    CALL STAGE1

.no_screen_change:

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

.no_arrows:
    BIT KB_SPACE, C			; La tecla presionada es SPACE
    call z,SHOOT_MAIN_CHAR

    BIT KB_DEL, C			; La tecla presionada es DEL    
    ret z

    jp MAIN_LOOP2

include "include\BTH_data.asm"

 PAGE 1
; CODE O NO

 PAGE 2

 PAGE 3

 PAGE 4
 PAGE 5
 PAGE 6

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
