; Dadas las coordenadas X,Y en pixels de dos sprites, devuelve 1 si hay colision
;Entrada:
;   b - Y SPR1
;   C - X SPR1

;   d - Y SPR2
;   e - X SPR2
;Salida:
;   a - bloque
;Modifica:
;   a, hl, de
check_spr_collision:        
        LD A,B          ; a = Y SPR1
        CP D
        JP NZ,.no_collision

        LD A,C          ; a = X SPR1
        CP E
        JP NZ,.no_collision

        LD A,1
        RET
.no_collision
    LD A,0
    RET

; Dadas las coordenadas X,Y en pixels, devuelve el bloque de 8x8 al que corresponde esas coordenadas
; La tabla de colisiones es la tabla de nombres con 32x24 = 756 bytes
;Entrada:
;   d - X
;   e - y
;Salida:
;   a - bloque
;Modifica:
;   a, hl, de
get_bloque_en_X_Y:
        ;(y/8)*32+(x/8)
        ld a,e      ;a=y
[3]     srl a       ;a=y/8
        ld h,0
        ld l,a      ;hl=y/8
[5]     add hl,hl   ;x32    ;a=(y/8)*32

        ld a,d      ;a=x
[3]     srl a       ;a=x/8
        ld d,0
        ld e,a      ;de=x/8
        add hl,de   ;hl=(y/8)*32+(x/8)

        ld de,mapa
        add hl,de   ;hl=mapa + hl=(y/8)*32+(x/8)

        ld a,[hl]
        ret

; SCREEN 8
SetVdp_Write:

	; transform address from 
	;
	; |           Register A            |           Register H            |           Register L            |
	; | --- --- --- --- --- --- --- A16 | A15 A14 A13 A12 A11 A10  A9  A8 |  A7  A6  A5  A4  A3  A2  A1  A0 |
	;
	; to
	;
	; |           Register A            |           Register H            |           Register L            |
	; | --- --- --- --- --- A16 A15 A14 | --- --- A13 A12 A11 A10  A9  A8 |  A7  A6  A5  A4  A3  A2  A1  A0 |
    rlc     h
    rla
    rlc     h
    rla
    srl     h
    srl     h

    di
	    ; write bits a14-16 of address to R#14
	    out     (PORT_1), a
	    ld      a, 14 + 128
	    out     (PORT_1), a

	    ; write the other address bits to VDP PORT_1
	    ld      a, l
	    nop
	    out     (PORT_1), a
	    ld      a, h
	    or      64
    ei
    out     (PORT_1),a
    ret

Set212Lines:
        ; set LN (bit 7) of R#9 to 1
        ld      a, (REG9SAV)
        or      10000000b
        ld      b, a
        ld      c, 9            ; register #
        call    WRTVDP
        ret

ClearVram_MSX2:
        xor     a           ; set vram write base address
        ld      hl, 0     	; to 1st byte of page 0
        call    SetVdp_Write
        xor		a

    ; TODO: 
    ;   use VDP command (currently is taking almost 1 second)
    ;   disable screen/sprites (should I ??)

    ; clear all 128kb of VRAM
	ld		d, 2		; 2 repetitions
.loop_2:
	ld		c, 0		; 256 repetitions
.loop_1:
	ld		b, 0		; 256 repetitions
.loop:
	out		(PORT_0), a
	djnz	.loop
	dec		c
	jp		nz, .loop_1
	dec		d
	jp		nz, .loop_2

	ret

; MEGAROM

opening_screen		
		ld		c,0
		ld		de,0
		call	_vdpsetvramwr
		ld	e,4
		ld	a, :GRAPHIC
		ld	d,a
2:		ld	(_bank2),a
		ld	hl,GRAPHIC
		ld	bc,0x98
		ld	a,32*2
1:		otir
		dec	a
		jr	nz,1b
		inc	d
		ld	a,d
		dec	e
		jr	nz,2b
		ret 

load_screen
		ld		c,0
		ld		de,0
		call	_vdpsetvramwr
		ld	e,2					; #pages
		ld	a, :BITMAP
		ld	d,a
2:		ld	(_bank2),a
		ld	hl,BITMAP
		ld	bc,0x98
		ld	a,64
1:		otir
		dec	a
		jr	nz,1b
		inc	d
		ld	a,d
		dec	e
		jr	nz,2b
		ret 

load_screen2
		ld		c,0
		ld		de,0
		call	_vdpsetvramwr
		ld	e,2					; #pages
		ld	a, b			; 1st page bitmap
		ld	d,a
2:		ld	(_bank2),a
		ld	hl,(BITMAP)		; Bitmap address
		ld	bc,0x98
		ld	a,64
1:		otir
		dec	a
		jr	nz,1b
		inc	d
		ld	a,d
		dec	e
		jr	nz,2b
		ret 

;Set VDP for writing at address CDE (17-bit) 

_vdpsetvramwr:
	ld a,c
	rlc d
	rla
	rlc d
	rla
	srl d ; primo shift, il secondo dopo la out

	out (0x99),a ;set bits 14-16
	ld a,14+128
	out (0x99),a
	srl d ; secondo shift.     
_vdpsetvramwr14
	ld a,e ;set bits 0-7
	out (0x99),a
	ld a,d ;set bits 8-13
	or 0x40 ; + write access
	out (0x99),a
	ret



		; --- RUTINAS PARA COLOCAR LAS PAGINAS DEL CARTUCHO ---
		; -----------------------------------------------------
		; --- SIEMPRE DEBEN IR EN LA PAGINA 1 DEL CARTUCHO! ---
		; -----------------------------------------------------

		; --- RUTINAS PRINCIPALES DEL MODULO ---
		; GETSLOT:	OBTIENE EL VALOR DEL SLOT QUE LE INDIQUEMOS
		; SETPAGES32K:	BIOS-ROM-YY-ZZ	 -> BIOS-ROM-ROM-ZZ (SITUA PAGINA 2)

		; --- VARIABLES EN RAM NECESARIAS ---
		; NINGUNA

GETSLOT:	; --- Rutina que construye el valor del SLOT para llamar a ENASLT ---
		; --- Entrada: a = SLOT                                           ---
		; --- Salida: a = valor para ENASLT                               ---
		; --- AUTOR: Konamiman                                            ---
		and	$03				; Proteccion, nos aseguramos de que el valor esta en 0-3
		ld	c,a				; c = slot de la pagina
		ld	b,0				; bc = slot de la pagina
		ld	hl,$FCC1			; Tabla de slots expandidos
		add	hl,bc				; hl -> variable que indica si este slot esta expandido
		ld	a,(hl)				; Tomamos el valor
		and	$80				; Si el bit mas alto es cero...
		jr	z,EXIT			; ...nos vamos a @@EXIT
		; --- El slot esta expandido ---
		or	c				; Slot basico en el lugar adecuado
		ld	c,a				; Guardamos el valor en c
		inc	hl				; Incrementamos hl una...
		inc	hl				; ...dos...
		inc	hl				; ...tres...
		inc	hl				; ...cuatro veces
		ld	a,(hl)				; a = valor del registro de subslot del slot donde estamos
		and	$0C				; Nos quedamos con el valor donde esta nuestro cartucho
EXIT:		or	c				; Slot extendido/basico en su lugar
		ret					; Volvemos

SETPAGES32K:	; --- Posiciona las paginas de un megarom o un 32K ---
		ld	a,$C9				; Codigo de RET
		ld	(NOPRET),a			; Modificamos la siguiente instruccion si estamos en RAM
NOPRET:	nop					; No hacemos nada si no estamos en RAM
		; --- Si llegamos aqui no estamos en RAM, hay que posicionar la pagina ---
		call	RSLREG  			; Leemos el contenido del registro de seleccion de slots
		rrca					; Rotamos a la derecha...
		rrca					; ...dos veces
		call	GETSLOT				; Obtenemos el slot de la pagina 1 ($4000-$BFFF)
		ld	h,$80				; Seleccionamos pagina 2 ($8000-$BFFF)
		jp	ENASLT ;ENASLT

SET_SCREEN5_MODE:
     ;Cambiamos el modo de pantalla
    ; BASIC: COLOR 15,1,1
    ; Establecer los colores
	ld hl,FORCLR ; Variable del Sistema
	ld [hl],15 ; Color del primer plano 15=blanco
	inc hl ; FORCLR+1
	ld [hl],1 ; Color de fondo 1=negro
	inc hl ; FORCLR+2
	ld [hl],1 ; Color del borde 1=negro

    ld  a,5     ; La rutina CHGMOD nos obliga a poner en el registro A el modo de pantalla que queremos
    call CHGMOD ; Mira arriba, pone la explicación
;
    ld a,(RG15AV) ; esta dirección de memoria almacena el valor del registro de lectura del VDP, mira arriba
    ;En or 0+0=0, 0+1=1, 1+1=1
    ;En and 0+0=0, 0+1=0, 1+1=1
    ;Con eso queremos cambiar los bits 7 y 8 del registro de escritura 1 del z80, queremos poner el 7 a 1 y también el 8 a 1
    ;el bit 7 del registro 1 pone los sprites en modo 16x16 (los que nostros queremos dibujar)
    ;el bit 8 queremos desactivarlo para no utilizar los sprites agrandados
    or 00000010b ; con or poniendo un 0 siempre respetamos los bits que hay y poniendo 1 1 obligamos a que sea 1
    ;and 11111110b ; con and obligamos a que el ultimo bit valga 0
    and 11111110b ; con and obligamos a que el ultimo bit valga 0

    ld b,a ;carga en b el valor de a
    ld c,1 ; La rutina WRTVDP necesta que le carguemos previamente el entero en C del z80 del registro que queroms modificar
    call WRTVDP ;Escribe en los registros del VDP 
    xor a ; ld a,0
	ld [CLIKSW],a ; Variable BIOS desactivar sonido teclas
    ret

DUMP_SPR_ALL:

;-----------------------------Definición del sprite en #7800  y volcado a la VRAM-------------------------------------------

    ld hl, SPRITE_PATTERN ; la rutina LDIRVM necesita haber cargado previamente la dirección de inicio de la RAM, para saber porqué he puesto 03800 fíjate este dibujo https://sites.google.com/site/multivac7/files-images/TMS9918_VRAMmap_G2_300dpi.png ,así es como está formado el VDP en screen 2
    ld de, #7800; la rutina necesita haber cargado previamente con de la dirección de inicio de la VRAM          
    ld bc, 8*4*12; 8 byte de cada tile * 4 que son los sprites de 16x16 y * #sprites
    call  LDIRVM ; Mira arriba, pone la explicación

;-----------------------------Definición de los atributos en #7600 y volcado a la VRAM------------------------------------

    ld hl, SPRITE_ATTRS ; la rutina LDIRVM necesita haber cargado previamente la dirección de inicio de la RAM, para saber porqué he puesto 0000 fíjate este dibujo https://sites.google.com/site/multivac7/files-images/TMS9918_VRAMmap_G2_300dpi.png ,así es como está formado el VDP en screen 2
    ld de, #7600; la rutina necesita haber cargado previamente con de la dirección de inicio de la VRAM          
    ld bc,128; 4 x #Sprites
    call  LDIRVM ; Mira arriba, pone la explicación

;-----------------------------Definición de los colores en #7400 y volcado a la VRAM------------------------------------

    ld hl, SPRITE_COLOR_TABLE ; la rutina LDIRVM necesita haber cargado previamente la dirección de inicio de la RAM, para saber porqué he puesto 0000 fíjate este dibujo https://sites.google.com/site/multivac7/files-images/TMS9918_VRAMmap_G2_300dpi.png ,así es como está formado el VDP en screen 2
    ld de, #7400; la rutina necesita haber cargado previamente con de la dirección de inicio de la VRAM          
    ld bc,16*10; 16 x #Sprites
    call  LDIRVM ; Mira arriba, pone la explicación
    ret
	
DUMP_SPR_ATTS:

;-----------------------------Definición de los atributos en #1E00 y volcado a la VRAM------------------------------------

    ld hl, SPRITE_ATTRS ; la rutina LDIRVM necesita haber cargado previamente la dirección de inicio de la RAM, para saber porqué he puesto 0000 fíjate este dibujo https://sites.google.com/site/multivac7/files-images/TMS9918_VRAMmap_G2_300dpi.png ,así es como está formado el VDP en screen 2
    ld de, #7600	; la rutina necesita haber cargado previamente con de la dirección de inicio de la VRAM          
    ld bc,20		; 5 x #Sprites
    call  LDIRVM 	; Mira arriba, pone la explicación

;	ld a, (SPRITE_COLOR_REPLACE2)
;	JP Z,.DUMP_SPR_ATTS_END
	ld hl, (SPRITE_COLOR_REPLACE2) ; la rutina LDIRVM necesita haber cargado previamente la dirección de inicio de la RAM, para saber porqué he puesto 0000 fíjate este dibujo https://sites.google.com/site/multivac7/files-images/TMS9918_VRAMmap_G2_300dpi.png ,así es como está formado el VDP en screen 2
    ld de, #7400; la rutina necesita haber cargado previamente con de la dirección de inicio de la VRAM          
    PUSH BC
	ld bc,16*3; 16 x #Sprites
    call  LDIRVM 
	POP BC

.DUMP_SPR_ATTS_END:
    ret

DUMP_SPR_P1:    
;-----------------------------Definición del sprite en #7800  y volcado a la VRAM-------------------------------------------

    ld hl, (SPRITE_PTR_REPLACE) ; la rutina LDIRVM necesita haber cargado previamente la dirección de inicio de la RAM, para saber porqué he puesto 03800 fíjate este dibujo https://sites.google.com/site/multivac7/files-images/TMS9918_VRAMmap_G2_300dpi.png ,así es como está formado el VDP en screen 2
    ld de, #7800; la rutina necesita haber cargado previamente con de la dirección de inicio de la VRAM          
    PUSH BC
	ld bc, 192; 32 bytes por sprite
    call  LDIRVM 
	POP BC

;-----------------------------Definición de los colores en #7400 y volcado a la VRAM------------------------------------

    ld hl, (SPRITE_COLOR_REPLACE) ; la rutina LDIRVM necesita haber cargado previamente la dirección de inicio de la RAM, para saber porqué he puesto 0000 fíjate este dibujo https://sites.google.com/site/multivac7/files-images/TMS9918_VRAMmap_G2_300dpi.png ,así es como está formado el VDP en screen 2
    ld de, #7400; la rutina necesita haber cargado previamente con de la dirección de inicio de la VRAM          
    PUSH BC
	ld bc,16*3; 16 x #Sprites
    call  LDIRVM 
	POP BC

; Replace 4 sprites(P1) in Sprite Pattern Table
;    xor a
;	di			; set VRAM address to 1800h
;	out (99h),a		; lower byte 00
;	ld a,78h + 040h		; upper byte 7800h with flag to set the bus in write mode (3800 VRAM: Sprite Pattern Table)
;	ei
;	out (99h),a		; note that this is protected by the ei instruction
;   push BC
;	ld bc,8098h     ; 80h = 128 bytes y port 98h 
;	ld hl,(SPRITE_PTR_REPLACE)

;.loop1:
;	outi			; send the data (HL) to port 98h 
;	jp nz,.loop1		; the inner loop is exactly 29 cycles. 
;   POP BC

    ; Replace 2 entries in sprite color table
;    xor a
;	di			; set VRAM address to 1800h
;	out (99h),a		; lower byte 00
;	ld a,74h + 040h		; upper byte 7400h with flag to set the bus in write mode (1C00 VRAM: Sprite Color Table)
;	ei
;	out (99h),a		; note that this is protected by the ei instruction
 ;   push BC
;	ld bc,2098h     ; 20h = 32 bytes y port 98h 
;	ld hl,(SPRITE_COLOR_REPLACE)
	
;.loop2:
;	outi			; send the data (HL) to port 98h 
;	jp nz,.loop2		; the inner loop is exactly 29 cycles. 
 ;   POP BC

    ret
