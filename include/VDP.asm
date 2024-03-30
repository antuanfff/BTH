print_string:       
	
	LD HL, string01
	LD DE, 5D28H	; Aquí irá el offset de la memoria del VDP en base a X, Y
.loop_str:
	LD A, (HL)
	AND A
	RET Z

	SUB 32
	RLC A
	RLC A	
    
	LD B, 0
	LD C, A	; 4 * 32 = 124, ultimo caractec primera linea. 1024 - primer caracter segunda linea
				; 16 * 4 = 64 -> numero 0 - ASCII 48 - 32 = 16 * 4 = 64 offset número 0
				; Si #ASCII > 63 -> 2ª línea
				; segunda línea - ((#ASCII -32)*4)+1024
	
	LD A, (HL)	; vuelvo a cargar el caracter para ver si está en la segunda línea (ascii > 63)
	PUSH HL
	LD HL, FONT
	ADD HL, BC
	CP 64
	JP C, .first_line_font
	LD BC, 1024	; le sumamos 1024 para saltar a la segunda línea de la fuente
	ADD HL, BC

.first_line_font:
    LD (BITMAP), HL
    
	LD B, :FONT
	PUSH DE
    call print_char
    POP DE
	
	LD IY, 4
	ADD IY, DE	; Le sumo 4 a DE para que empiece a escribir el next char 8 pixels a la derecha 
	LD D, IYH
	LD E, IYL
	

	POP HL
	INC HL
	JR .loop_str
    
    RET

print_char
		XOR A		
		ld		c,0
		;ld		de,0
		LD (CHR_ACR), DE
    	call	_vdpsetvramwr		
		ld	a, b			; 1st page bitmap
		;ld	d,a
		ld	(_bank2),a
		ld	hl,(BITMAP)		; Bitmap address
		ld	a, 8			; #lineas del caracter
1:      ld	bc,0x498		; escribimos 4 bytes en el puerto 98h	
        otir
        LD BC, 124
        ADD HL, BC
		PUSH AF

		LD IY, (CHR_ACR)
		LD BC, 128
		ADD IY, BC
		LD D, IYH
		LD E, IYL
		XOR C
		LD (CHR_ACR), DE
		call	_vdpsetvramwr		
		POP AF		

		dec	a
		jr	nz,1b		
		ret 
