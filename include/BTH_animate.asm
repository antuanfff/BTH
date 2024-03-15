; Movimiento P1
move_up:
    XOR C               ; Reseteamos la entrada del teclado
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
    ;JP no_arrows
    ret

move_down:
    XOR C               ; Reseteamos la entrada del teclado
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
    ;JP no_arrows
    ret

move_right:
    XOR C               ; Reseteamos la entrada del teclado
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
    ;JP no_arrows
    ret

move_left:
    XOR C               ; Reseteamos la entrada del teclado
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
    ;JP no_arrows
    ret

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
    ret
