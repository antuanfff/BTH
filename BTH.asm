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
	include "include\SETPAGES48K.asm"

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
    LD HL, CEMENTER1
    LD (BITMAP), HL
    LD B, :CEMENTER1
    call load_screen
    
    call INIT_CHARS_VARS
    LD A, -MOV_SPEED_GHOST
	LD (CHAR_SPEED_X_GHOST), A
    call DUMP_SPR_ALL
    LD HL, mapa1
    LD (MAPA), HL
    call MAIN_LOOP
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
    LD A,(CHAR_MAIN_SHOOT)    
    CP $01
    JP Z,.MOVE_SHOOT_LEFT
    CP $02
    JP Z,.MOVE_SHOOT_RIGHT
    CP $03
    JP Z,.MOVE_SHOOT_UP    
    CP $04
    JP Z,.MOVE_SHOOT_DOWN
    JP .check_KB

.MOVE_SHOOT_RIGHT:        
    LD A, MOV_SPEED_SHOOT
	LD (CHAR_SPEED_SHOOT), A    
    JP .CHECK_SHOOT_DISTANCE

.MOVE_SHOOT_UP:        
    LD A, -MOV_SPEED_SHOOT
	LD (CHAR_SPEED_SHOOT), A    
    JP .CHECK_SHOOT_DISTANCE

.MOVE_SHOOT_DOWN:
    LD A, MOV_SPEED_SHOOT
	LD (CHAR_SPEED_SHOOT), A    
    JP .CHECK_SHOOT_DISTANCE

.MOVE_SHOOT_LEFT:    
    LD A, -MOV_SPEED_SHOOT
	LD (CHAR_SPEED_SHOOT), A    

.CHECK_SHOOT_DISTANCE:
    ; Miramos si va a izq o der
    LD A,(CHAR_MAIN_SHOOT)    
    SUB 3   ; Restar 3 a 1 o 2 provoca salto de carro, si es 3 o 4 no provoca el salto de carro
    JP NC,.ADD_SHOOT_Y
    ; Movemos el disparo
    LD A, (ix+17)          ;cargamos la X del disparo
	LD HL, (CHAR_SPEED_SHOOT)
	ADD L					; Actualizamos la posicion en base a la velocidad
    LD (ix+17), A
    JP .CHECK_GHOST
.ADD_SHOOT_Y
    ; Movemos el disparo
    LD A, (ix+16)          ;cargamos la X del disparo
	LD HL, (CHAR_SPEED_SHOOT)
	ADD L					; Actualizamos la posicion en base a la velocidad
    LD (ix+16), A

.CHECK_GHOST:
    ; Comprobamos si hay colision con el fantasma
    LD A,(CHAR_GHOST_DEAD)  ; si está muerto,no lo miramos
    CP $01
    JP Z,.ADD_DISTANCE

    LD B,(ix+16)         ; Y del disparo
    LD C,(ix+17)         ; X del disparo
    LD D,(ix+12)         ; Y del fantasma 
    LD E,(ix+13)         ; X del fantasma
    call check_spr_collision
    CP 1
    JP Z,.KILL_GHOST
.ADD_DISTANCE:
    LD A,(CHAR_DISTANCE_SHOOT)
    ADD MOV_SPEED_SHOOT
    LD (CHAR_DISTANCE_SHOOT),A    
    CP MAX_DISTANCE_SHOOT
    JP NZ,.check_KB
    JP .HIDE_SHOOT

.KILL_GHOST:
    LD A,1
    LD (CHAR_GHOST_DEAD),A
    LD (ix+12),217    

.HIDE_SHOOT:
    LD (ix+16),217          ; Y = 217 para ocultar el Sprite
    XOR A
    LD (CHAR_MAIN_SHOOT),A   ; Desactivo el estado disparando 
    LD (CHAR_DISTANCE_SHOOT),A

.check_KB:
    halt    

    ld a, 8
	call SNSMAT   
    LD C,A    
    
    BIT KB_RIGHT, C			; La tecla presionada es DOWN?
    jp z, move_right
    
    BIT KB_LEFT, C			; La tecla presionada es DOWN?
    jp z, move_left

    BIT KB_UP, C			; La tecla presionada es UP?
    jp z, move_up

    BIT KB_DOWN, C			; La tecla presionada es DOWN?
    jp z, move_down

no_arrows:
    BIT KB_SPACE, C			; La tecla presionada es SPACE
    call z,SHOOT_MAIN_CHAR

    BIT KB_DEL, C			; La tecla presionada es DEL    
    ret z

    jp MAIN_LOOP

SHOOT_MAIN_CHAR:    
    LD A, (CHAR_MAIN_SHOOT)
    CP $01                  ; Si ya está disparando esperamos a que termine
    JP Z,MAIN_LOOP
    CP $02                  ; Si ya está disparando esperamos a que termine
    JP Z,MAIN_LOOP
    CP $03                  ; Si ya está disparando esperamos a que termine
    JP Z,MAIN_LOOP
    CP $04                  ; Si ya está disparando esperamos a que termine
    JP Z,MAIN_LOOP

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
    jp MAIN_LOOP

move_up:
    ; Actualizamos la última tecla de dirección pulsada
    XOR A
    LD (CHAR_NEW_DIR_MAIN),A
    CALL CHECK_DIRECTION_MAIN

    ld a,(ix); obtenemos el valor actual de la posicion y
    sub 1 ; decrementamos en 1 el valor, aunque realmente será dos por el desplazamiento de la Y que hace el VDP
    ld e,a  ; Metemos el parametro Y para verificar si hay colision
    ld a,(ix+1)
    ld d,a  ; Metemos el parametro X para verificar si hay colision
    call get_bloque_en_X_Y
    cp 1
    jp z,NO_MOVEMENT

    ld a,(ix); obtenemos el valor actual de la posicion y
    sub 1 ; decrementamos en 1 el valor, aunque realmente será dos por el desplazamiento de la Y que hace el VGP
    ld e,a  ; Metemos el parametro Y para verificar si hay colision
    ld a,(ix+1)
    add 10 ; miramos la segunda tile 8+2
    ld d,a  ; Metemos el parametro X para verificar si hay colision
    call get_bloque_en_X_Y
    cp 1
    jp z,NO_MOVEMENT
      
	LD A, -MOV_SPEED
	LD (CHAR_SPEED_Y), A
    call UPDATE_MOVEMENT   
    JP no_arrows
    ;ret

move_down:    
    ; Actualizamos la última tecla de dirección pulsada
    LD A,$01
    LD (CHAR_NEW_DIR_MAIN),A
    CALL CHECK_DIRECTION_MAIN

    ld a,(ix); obtenemos el valor actual de la posicion y
    add 17 ; incrementamos en 15+2 el valor
    ld e,a  ; Metemos el parametro Y para verificar si hay colision
    ld a,(ix+1)
    ld d,a  ; Metemos el parametro X para verificar si hay colision
    call get_bloque_en_X_Y
    cp 1
    jp z,NO_MOVEMENT

    ld a,(ix); obtenemos el valor actual de la posicion y
    add 17 ; incrementamos en 15 el valor
    ld e,a  ; Metemos el parametro Y para verificar si hay colision
    ld a,(ix+1)
    add 10 ; miramos la segunda tile 8+2
    ld d,a  ; Metemos el parametro X para verificar si hay colision    
    call get_bloque_en_X_Y
    cp 1
    jp z,NO_MOVEMENT
    
    LD A, MOV_SPEED
	LD (CHAR_SPEED_Y), A
    call UPDATE_MOVEMENT    
    JP no_arrows
    ;ret

move_right:
    ; Actualizamos la última tecla de dirección pulsada
    LD A,$03
    LD (CHAR_NEW_DIR_MAIN),A
    CALL CHECK_DIRECTION_MAIN

    ld a,(ix+1); obtenemos el valor actual de la posicion x   
    add 16; incrementamos en 3 el valor
    ld d,a  ; Metemos el parametro X para verificar si hay colision
    ld a,(ix)   ;obtenemos el valor actual de la posicion Y
    add 1   ; incrementamos en 1 por el desplazamiento del VGP
    ld e,a  ; Metemos el parametro Y para verificar si hay colision
    call get_bloque_en_X_Y
    cp 1
    jp z,NO_MOVEMENT

    ;Miramos el tile del byte inferior del sprite - centro personaje
    ld a,(ix+1); obetenemos el valor actual de la posicion x   
    add 16; incrementamos en 2 tiles el valor
    ld d,a  ; Metemos el parametro X para verificar si hay colision
    ld a,(ix) ;obtenemos el valor actual de la posicion y
    add 8 ; Añadimos 8 para mirar el tile inferior - centro
    ld e,a  ; Metemos el parametro Y para verificar si hay colision
    call get_bloque_en_X_Y
    cp 1
    jp z,NO_MOVEMENT

    ;Miramos el tile del byte inferior del sprite - esquina inferior derecha
    ld a,(ix+1); obetenemos el valor actual de la posicion x   
    add 16; incrementamos en 2 tiles el valor
    ld d,a  ; Metemos el parametro X para verificar si hay colision
    ld a,(ix) ;obtenemos el valor actual de la posicion y
    add 15 ; Añadimos 8+1 para mirar la esquina inferior derecha del tile inferior
    ld e,a  ; Metemos el parametro Y para verificar si hay colision
    call get_bloque_en_X_Y
    cp 1
    jp z,NO_MOVEMENT    
    
    LD A, MOV_SPEED
	LD (CHAR_SPEED_X), A
    call UPDATE_MOVEMENT      
    JP no_arrows
    ;ret

move_left:
    ; Actualizamos la última tecla de dirección pulsada
    LD A,$02
    LD (CHAR_NEW_DIR_MAIN),A
    CALL CHECK_DIRECTION_MAIN
    ld a,(ix+1); obetenemos el valor actual de la posicion x
    sub 2 ; decrementamos en 2 el valor
    ld d,a  ; Metemos el parametro X para verificar si hay colision
    ld a,(ix)   ;obtenemos el valor actual de la posicion Y
    add 1   ; incrementamos en 1 por el desplazamiento del VGP
    ld e,a  ; Metemos el parametro Y para verificar si hay colision
    call get_bloque_en_X_Y
    cp 1
    jp z,NO_MOVEMENT

    ;Miramos el tile inferior parte central del personaje
    ld a,(ix+1); obetenemos el valor actual de la posicion x
    sub 2 ; decrementamos en 2 el valor
    ld d,a  ; Metemos el parametro X para verificar si hay colision
    ld a,(ix)   ;obtenemos el valor actual de la posicion Y
    add 8   ; incrementamos en 8 para mirar la parte central del personaje
    ld e,a  ; Metemos el parametro Y para verificar si hay colision
    call get_bloque_en_X_Y
    cp 1
    jp z,NO_MOVEMENT

    ;Miramos el tile inferior esquina inferior izquierda
    ld a,(ix+1); obetenemos el valor actual de la posicion x
    sub 2 ; decrementamos en 2 el valor
    ld d,a  ; Metemos el parametro X para verificar si hay colision
    ld a,(ix)   ;obtenemos el valor actual de la posicion Y
    add 15   ; incrementamos en 8 para mirar la parte central del personaje (-1 por el offset del VGP)
    ld e,a  ; Metemos el parametro Y para verificar si hay colision
    call get_bloque_en_X_Y
    cp 1
    jp z,NO_MOVEMENT

    LD A, -MOV_SPEED
	LD (CHAR_SPEED_X), A
    call UPDATE_MOVEMENT     
    JP no_arrows
    ;ret

CHECK_DIRECTION_MAIN:
    LD A, (CHAR_DIR_MAIN)		                ;Cargamos el valor anterior de direccion
	LD HL, (CHAR_NEW_DIR_MAIN)		            ;Cargamos el valor actual de direccion
	CP L		            	                ;Son iguales?
	RET Z                           			;Si son iguales, vuelvo
    LD A,(CHAR_NEW_DIR_MAIN)
    CP $01
    JP Z,.SET_DIR_DOWN
    CP $00
    JP Z,.SET_DIR_UP
    CP $03
    JP Z,.SET_DIR_RIGHT
    CP $02
    JP Z,.SET_DIR_LEFT
    JP .FINISH
    
.SET_DIR_DOWN:    
    ld hl, SPRITE_P1_DOWN
    ld (SPRITE_PTR_REPLACE), HL
    
    ld hl, SPRITE_COLOR_P1_DOWN
    ld (SPRITE_COLOR_REPLACE), HL    
    ld (SPRITE_COLOR_REPLACE2), HL
    CALL DUMP_SPR_P1    

    JP .FINISH

.SET_DIR_UP:
    ld hl, SPRITE_P1_UP
    ld (SPRITE_PTR_REPLACE), HL
    
    ld hl, SPRITE_COLOR_P1_UP
    ld (SPRITE_COLOR_REPLACE), HL
    ld (SPRITE_COLOR_REPLACE2), HL
    CALL DUMP_SPR_P1
    JP .FINISH

.SET_DIR_LEFT:    
    ld hl, SPRITE_P1_LEFT
    ld (SPRITE_PTR_REPLACE), HL
    
    ld hl, SPRITE_COLOR_P1_LEFT
    ld (SPRITE_COLOR_REPLACE), HL
    ld (SPRITE_COLOR_REPLACE2), HL
    CALL DUMP_SPR_P1   
    JP .FINISH

.SET_DIR_RIGHT:
    ld hl, SPRITE_P1_RIGHT
    ld (SPRITE_PTR_REPLACE), HL
    
    ld hl, SPRITE_COLOR_P1_RIGHT
    ld (SPRITE_COLOR_REPLACE), HL
    ld (SPRITE_COLOR_REPLACE2), HL
    ;    ld (ix+13), $10      ; Sprite 1 - Ghost
    CALL DUMP_SPR_P1
    JP .FINISH

.FINISH:
    XOR A   ; reseteamos el contador de pasos
    LD (CHAR_MIN_STEP), A   
    LD A, (CHAR_NEW_DIR_MAIN)   ; Actualizamos la nueva dirección del personaje
    LD (CHAR_DIR_MAIN),A    

    ; ponemos el primer frame del sprite
    LD (ix+2), $00
    LD (ix+6), $04
    LD (ix+10), $08
    ;CALL DUMP_SPR_ATTS
    RET

UPDATE_MOVEMENT:
	LD A, (ix)          ;cargamos la Y			
	LD HL, (CHAR_SPEED_Y)
	ADD L					; Actualizamos la posicion en base a la velocidad
    
	LD (ix), A
    LD (ix+4), A
    LD (ix+8), A
	
	LD A, (ix+1)			;cargamos la X
	LD HL, (CHAR_SPEED_X)
	ADD L   					; Actualizamos la posicion en base a la velocidad
    
	LD (ix+1), A
    LD (ix+5), A
    LD (ix+9), A	

    XOR A            ; reseteamos las variables de movimiento para el siguiente ciclo
    LD (CHAR_SPEED_X),A
    LD (CHAR_SPEED_Y),A	

    ; Animación/pasos de personaje	
    LD A, (CHAR_MIN_STEP)
    CP MAX_CHAR_STEPS
    JP NZ,.NO_CHAR_PATTERN_CHANGE
    
    XOR A
    LD (CHAR_MIN_STEP), A   ; reseteamos el contador de pasos
    LD A, (ix+2)    ; Cargamos el patrón
    CP $00
    JP Z,.SET_SPR_1P_2
    LD (ix+2), $00
    LD (ix+6), $04
    LD (ix+10), $08
    ld hl, (SPRITE_COLOR_REPLACE)
    ld (SPRITE_COLOR_REPLACE2), HL
    ret

.SET_SPR_1P_2:
    LD (ix+2), $0C
    LD (ix+6), $10
    LD (ix+10), $14
        
    ld HL, (SPRITE_COLOR_REPLACE)
    ld DE,48
    ADD HL, DE
    ld (SPRITE_COLOR_REPLACE2), HL
    ;ld a, 0
	ret

.NO_CHAR_PATTERN_CHANGE    
    ADD 1
    LD (CHAR_MIN_STEP), A    
    ret

NO_MOVEMENT:    
    
    call MAIN_LOOP
    ret

STAGE2:
    LD HL, CEMENTER2
    LD (BITMAP), HL
    LD B, :CEMENTER2

    call load_screen
    LD HL, mapa2
    LD (MAPA), HL
    
MAIN_LOOP2:
    halt    

    ld a, 8
	call SNSMAT   
    LD C,A    
    
    BIT KB_RIGHT, C			; La tecla presionada es DOWN?
    jp z, move_right
    
    BIT KB_LEFT, C			; La tecla presionada es DOWN?
    jp z, move_left

    BIT KB_UP, C			; La tecla presionada es UP?
    jp z, move_up

    BIT KB_DOWN, C			; La tecla presionada es DOWN?
    jp z, move_down

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
