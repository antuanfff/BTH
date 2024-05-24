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
    call ANIMATE_P1
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
    call ANIMATE_P1
    ;JP no_arrows
    ret

move_right:
    XOR C               ; Reseteamos la entrada del teclado
    ; Actualizamos la última tecla de dirección pulsada
    BIT KB_UP, C
    JR z, .KB_UPDOWN_PRESSED
    BIT KB_DOWN, C
    JR z, .KB_UPDOWN_PRESSED
    LD A,$03
    LD (CHAR_NEW_DIR_MAIN),A
    CALL CHECK_DIRECTION_MAIN

.KB_UPDOWN_PRESSED:
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
    BIT KB_UP, C
    ret z
    BIT KB_DOWN, C
    ret z
    call ANIMATE_P1    
    ret

move_left:
    XOR C               ; Reseteamos la entrada del teclado
    ; Actualizamos la última tecla de dirección pulsada
    BIT KB_UP, C
    JR z, .KB_UPDOWN_PRESSED
    BIT KB_DOWN, C
    JR z, .KB_UPDOWN_PRESSED
    LD A,$02
    LD (CHAR_NEW_DIR_MAIN),A
    CALL CHECK_DIRECTION_MAIN
.KB_UPDOWN_PRESSED:
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
    BIT KB_UP, C
    ret z
    BIT KB_DOWN, C
    ret z
    call ANIMATE_P1    
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
    RET

ANIMATE_P1:
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

MOVE_SHOOT:
    LD A,(CHAR_MAIN_SHOOT)    
    CP $01
    JP Z,.MOVE_SHOOT_LEFT
    CP $02
    JP Z,.MOVE_SHOOT_RIGHT
    CP $03
    JP Z,.MOVE_SHOOT_UP    
    CP $04
    JP Z,.MOVE_SHOOT_DOWN
    RET

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
    LD A, (ix+SPR_SHOOT_P1+1)          ;cargamos la X del disparo
	LD HL, (CHAR_SPEED_SHOOT)
	ADD L					; Actualizamos la posicion en base a la velocidad
    LD (ix+SPR_SHOOT_P1+1), A
    JP .ADD_DISTANCE
.ADD_SHOOT_Y
    ; Movemos el disparo
    LD A, (ix+SPR_SHOOT_P1)          ;cargamos la X del disparo
	LD HL, (CHAR_SPEED_SHOOT)
	ADD L					; Actualizamos la posicion en base a la velocidad
    LD (ix+SPR_SHOOT_P1), A

.ADD_DISTANCE:
    LD A,(CHAR_DISTANCE_SHOOT)
    ADD MOV_SPEED_SHOOT
    LD (CHAR_DISTANCE_SHOOT),A    
    CP MAX_DISTANCE_SHOOT
    RET NZ

.HIDE_SHOOT:
    LD (ix+SPR_SHOOT_P1),217          ; Y = 217 para ocultar el Sprite
    XOR A
    LD (CHAR_MAIN_SHOOT),A   ; Desactivo el estado disparando 
    LD (CHAR_DISTANCE_SHOOT),A
    RET

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

BOUNCE_ANDY:
    LD A, (ix)
    ADD 16
    LD (ix), A
    LD (ix+4), A
    LD (ix+8), A

    ret