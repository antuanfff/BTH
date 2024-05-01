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
		RET

; -----------------------------------------------------------------------------------
; https://www.msx.org/forum/development/msx-development/assembly-combined-basic
; https://problemkaputt.de/portar.htm#vdpregisters20h2ehmsx2videocommandregisters
; -----------------------------------------------------------------------------------
VDP_01: EQU   $F3E0
VDP_08: EQU   $FFE7
VDP_09: EQU   $FFE8

SETPAG:				; SETPAG [A]
	RRCA
	RRCA
	RRCA
	OR	%00011111
	DI
	OUT	($99),A
	LD	A,$80+2
	EI
	OUT	($99),A
	RET

SET_WR:				; SET_WR [AHL]
	RLC	H
	RLA
	RLC	H
	RLA
	SRL	H
	SRL	H
	DI
	OUT	($99),A
	LD	A,$80+14
	OUT	($99),A
	LD	A,L
;	NOP				; MSX2+
	OUT	($99),A
	LD	A,H
	OR	64
	OUT	($99),A
	EI
	RET

SET_RD:				; SET_RD [AHL]
	RLC	H
	RLA
	RLC	H
	RLA
	SRL	H
	SRL	H
	DI
	OUT	($99),A
	LD	A,$80+14
	OUT	($99),A
	LD	A,L
;	NOP				; MSX2+
	OUT	($99),A
	LD	A,H
;	NOP				; MSX2+
	OUT	($99),A
	EI
	RET

VDPCMD:				; VDPCMD [HL]->[CMDTABLE]
	CALL	WAITCE
	DI
	LD	A,$20
	OUT	($99),A
	LD	A,$80+17
	OUT	($99),A
	EI
	LD	BC,$0F9B
	OTIR
	RET

WAITCE:				; WAITCE
	LD	A,$02
	DI
	OUT	($99),A
	LD	A,$80+15
	OUT	($99),A
	IN	A,($99)
	RRA
	LD	A,$00
	OUT	($99),A
	LD	A,$80+15
	EI
	OUT	($99),A
	JR	C,WAITCE
	RET

ENASCR:				; Enable Screen
	LD	A,(VDP_01)
	OR	%01000000
	JR	DISSCR.OUT

DISSCR:				; Disable Screen
	LD	A,(VDP_01)
	AND	%10111111
.OUT:	LD	(VDP_01),A
	DI
	OUT	($99),A
	LD	A,$80+1
	EI
	OUT	($99),A
	RET

ENASPR:				; Enable Sprites
	LD	A,(VDP_08)
	AND	%11111101
	JR	DISSPR.OUT

DISSPR:				; Disable Sprites
	LD	A,(VDP_08)
	OR	%00000010
.OUT:	LD	(VDP_08),A
	DI
	OUT	($99),A
	LD	A,$80+8
	EI
	OUT	($99),A
	RET

LIN192:				; Set 192 lines
	LD	A,(VDP_09)
	OR	%10000000
	JR	LIN212.OUT

LIN212:				; Set 212 lines
	LD	A,(VDP_09)
	AND	%01111111
.OUT:	LD	(VDP_09),A
	DI
	OUT	($99),A
	LD	A,$80+9
	EI
	OUT	($99),A
	RET