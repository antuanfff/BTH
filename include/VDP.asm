print_strings_dialog_box:

	LD H, (IY+1)
	LD L, (IY)
	LD DE, FIRST_LINE_DLG_BOX	; Aquí irá el offset de la memoria del VDP en base a X, Y
	PUSH IY
    call print_string 
    POP IY

	LD H, (IY+3)
	LD L, (IY+2)
    LD DE, SEC_LINE_DLG_BOX	; Aquí irá el offset de la memoria del VDP en base a X, Y    
	PUSH IY
    call print_string 
	POP IY

    LD H, (IY+5)
	LD L, (IY+4)
	LD DE, THIRD_LINE_DLG_BOX	; Aquí irá el offset de la memoria del VDP en base a X, Y
    call print_string 
	RET

print_string:       
	
	;LD HL, string01
	;LD DE, 5CA8H	; Aquí irá el offset de la memoria del VDP en base a X, Y
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

CLEAR_DIALOG_BOX:		
		LD C,0
		LD DE, 5C28H
		LD (CHR_ACR), DE
		call _vdpsetvramwr
		LD A, 24		; Borraremos 24 líneas de la pantalla
1:		LD HL, BLANK_DATA
		LD BC,0x5498	; Escribimos 84 bytes
		OTIR

		PUSH AF
		LD IY, (CHR_ACR)
		LD BC, 128
		ADD IY, BC
		LD D, IYH
		LD E, IYL
		LD C,0
		LD (CHR_ACR), DE
		call	_vdpsetvramwr		
		POP AF		

		DEC a
		JR NZ,1b
		XOR A
    	LD (SHOWING_DIALOG), A
		RET
