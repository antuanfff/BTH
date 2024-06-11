; Offset commands registers
VDP_SX		 EQU 0
VDP_SY		 EQU 2
VDP_DX		 EQU 4
VDP_DY		 EQU 6
VDP_NX		 EQU 8
VDP_NY		 EQU 10
VDP_COLOR	 EQU 12
VDP_ARGUMENT EQU 13
VDP_COMMAND	 EQU 14

; Offset for sprite commands from memory

; VDP Commands
CMD_YMMM	equ	$e0
CMD_HMMM	equ	$d0
CMD_HMMV	equ	$c0
CMD_LMMM	equ	$98
CMD_LMMC	equ $b0
CMD_LMMV    equ $80

; Logical operations
VDP_IMP		equ	%0000
VDP_AND		equ	%0001
VDP_OR		equ	%0010
VDP_XOR		equ	%0011
VDP_NOT		equ	%0100
VDP_TIMP	equ	%1000
VDP_TAND	equ	%1001
VDP_TOR		equ	%1010
VDP_TXOR	equ	%1011
VDP_TNOT	equ	%1100

; Tile
ENERGY_WIDTH		equ	16		; Blood drops
TILE_WIDTH			equ	32
TILE_HEIGHT			equ	16
TILES_VDP_PAGE			equ	1		; Page where tiles are stored
TILES_START_ADDR 	equ $8000  ; Tiles in ROM will be loaded at $8000, so we can load them to VRAM
BACK_BUFFER			equ 2		; we will draw to page 1
FRONT_BUFFER		equ 0		; then copy to page 0

; Tile Map
TILENUM_OFFSET	equ 0
DX_OFFSET	equ 1
DY_OFFSET	equ 2
REPS_OFFSET	equ 3
TILEMAP_SIZE	equ 4

; Metatiles
METATILE_SX		equ 0
METATILE_SY		equ 1
METATILE_NX		equ 2
METATILE_NY		equ 3
METATILE_SIZE	equ 4

; Font
FONT_HEIGHT			equ 8
FONT_WIDTH			equ 8
FONT_Y_OFFSET_1L	equ 212
FONT_Y_OFFSET_2L	equ 220

; Dialog Box
DIAGBOX_HEIGHT	equ 20
DIAGBOX_WIDTH	equ 255

;---------------------------------------------------------------------------
; Init the RAM buffers used to draw a tile, energy and entities
;---------------------------------------------------------------------------
initVDPBuffers:
		ld	hl,charDatROM
		ld	de,charDat
		ld	bc,15
		ldir

		ld	hl,tileDatROM
		ld	de,tileDat
		ld	bc,15
		ldir
		
		ld	hl,tileDatTransROM
		ld	de,tileDatTrans
		ld	bc,15
		ldir	

		ld	hl,energyDatROM
		ld	de,energyDat
		ld	bc,15
		ldir	

		ld 	hl, init_playerDAT
		ld de, ENTITY_PLAYER_POINTER
		ld bc,ENTITY_SIZE
		ldir

		ld 	hl, enemy1_stg1DAT
		;add hl, 9			; ENTITY_SIZE
		ld de, ENTITY_ENEMY1_POINTER
		ld bc,ENTITY_SIZE
		ldir

		ret

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
		;ld	(_bank2),a
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

CLEAR_DIALOG_BOX_v1:		
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

CLEAR_DIALOG_BOX_v2:
		LD C,0
		LD DE, FIRST_LINE_DLG_BOX
		LD (CHR_ACR), DE
		call _vdpsetvramwr
		LD A, 24		; Borraremos 24 líneas de la pantalla
1:		LD HL, BLANK_DATA
		LD BC,0x8098	; Escribimos 128 bytes, la linea entera
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
	
CLEAR_DIALOG_BOX:
		LD HL, DiagBoxToFrontkBufROM
		CALL VDPCMD
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
;
; This lil' routine waits until the VDP is done copying.
;
VDP_Ready:
    ld a,2
    di
    out (#99),a     ; select s#2
    ld a,15+128
    out (#99),a
    in a,(#99)
    rra
    ld a,0          ; back to s#0, enable ints
    out (#99),a
    ld a,15+128
    ei
    out (#99),a     ; loop if vdp not ready (CE)
    jp c,VDP_Ready
    ret

;INPUT: A - ANDY'S MAX ENERGY
DRAW_ANDY_ENERGY:		
	LD DE, 0
	LD IY, energyDat
    ;LD (IY + VDP_SX), 128      ; SXL - Tile 4
    ;LD (IY+VDP_SY), 0      ; SYL	
    ;LD (IY + VDP_DX), A     ; DXL    
    LD (IY + VDP_DY), 194      ; DYL    
	LD A, (ENTITY_PLAYER_POINTER+ENTITY_ENERGY)		; cargamos la energia de Andy
	CP 0
	JR Z, .draw_empty_drops
	;LD A, 24  ; TESTS	

.check_next_drop
	CP 4
	JP Z, .draw_half_drop	
	LD (IY + VDP_SX), 128      ; SXL - Tile 4
	LD (IY + VDP_DX), D     ; DXL    
	PUSH AF
	LD A, D
	ADD A, 16
	LD D, A 	
	LD HL, energyDat
    CALL VDPCMD
	INC E
	POP AF
	SUB 8
	JP NZ, .check_next_drop
	;ret
	JR .draw_empty_drops

.draw_half_drop		
	LD (IY + VDP_SX), 144      ; SXL - Tile 5
	LD (IY + VDP_DX), D     ; DXL    
	LD A, D
	ADD A, 16
	LD D, A 	
    LD HL, energyDat
    CALL VDPCMD
	INC E
	;ret

.draw_empty_drops
		LD HL, ANDY_MAX_ENERGY
		LD BC, (current_level)
		LD B,0
		ADD HL, BC
		;INC HL
		LD A, (HL)
		;LD A, 24
[3]     srl a       ;a/8
		sub e
		ret Z		; si es cero volvemos, full energy
.loop
		LD (IY + VDP_SX), 160      ; SXL - Tile 6
		LD (IY + VDP_DX), D     ; DXL    
    	LD HL, energyDat
		PUSH AF
		LD A, D
		ADD A, 16
		LD D, A 			
    	CALL VDPCMD
		pop AF
		dec a
		jp nz, .loop
		ret
		;ld a, (ENTITY_PLAYER_POINTER+3)	; Andy's energy

; Draw a background tile - Background
; Input:
; A - Number of tile
; D - X
; E - Y
draw_tile:
	LD IY, tileDat
	JP draw_tile_common

; Draw a background tile - Transparent
; Input:
; A - Number of tile
; D - X
; E - Y
draw_tile_trans:
	LD IY, tileDatTrans
	
draw_tile_common:
	LD HL, metatiles_data
	ADD A,A
	ADD A,A ; A*4 (size of metatiles data)
	LD B, 0
	LD C, A
	ADD HL, BC
	;LD IY, tileDat
	LD A, (HL)
	LD (IY + VDP_SX), A
	INC HL
	LD A, (HL)
	LD (IY + VDP_SY), A
	INC HL
	LD A, (HL)
	LD (IY + VDP_NX), A
	INC HL
	LD A, (HL)
	LD (IY + VDP_NY), A
	LD (IY + VDP_DX), D
	LD (IY + VDP_DY), E
	PUSH IY
	POP HL
	CALL VDPCMD
	ret

print_strings_dialog_box_v2:

	LD H, (IY+1)
	LD L, (IY)
	LD A, FIRST_LINE_DLG_BOX_v2	; Aquí irá el offset de la memoria del VDP en base a X, Y
	PUSH IY
    call print_string_v2
    POP IY

	LD H, (IY+3)
	LD L, (IY+2)
    LD A, SEC_LINE_DLG_BOX_v2	; Aquí irá el offset de la memoria del VDP en base a X, Y    
	;PUSH IY
    call print_string_v2
	;POP IY

	RET

print_string_v2:	
	
	LD IY, charDat
	LD (IY+VDP_DY), A
	LD D, 0		; acumulador de la DX
.loop_str:
	
	LD A, (HL)
	AND A		; if A = 0 -> RET
	RET Z	
	CP 64		 ; Si #ASCII > 63 -> 2ª línea
	JP C, .first_line_font
	LD (IY+VDP_SY), FONT_Y_OFFSET_2L		; Cargamos la SY	
	JR .continue

.first_line_font:
    LD (IY+VDP_SY), FONT_Y_OFFSET_1L		; Cargamos la SY	
    
.continue:
	SUB 32
	RLC A
	RLC A	
	RLC A		; numero 0: ASCII #48 - 32 = 16 * 8 = SX número 0, Y = 0	
	
	LD (IY+VDP_SX), A		; Cargamos la SX
	LD (IY+VDP_DX), D		; Cargamos la DX
	
	PUSH HL
	LD HL, charDat
	CALL VDPCMD	
	
	LD A, D
	ADD FONT_WIDTH	; Le sumo 8 para que empiece a escribir el next char 8 pixels a la derecha 
	LD D, A	

	POP HL
	INC HL
	JR .loop_str
    
    RET

; Loads the screen image using the tile map

load_screen_v2:	
	LD IY, (stg_map_ptr_back)

.map_element	
	LD A, (IY+TILENUM_OFFSET)
	LD HL, metatiles_data
	ADD A,A
	ADD A,A ; A*4 (size of metatiles data)
	ADD A, METATILE_NX
	LD B, 0
	LD C, A
	ADD HL, BC
	
	LD C, (HL)	; Guardamos el ancho de la tile
	
	LD D, (IY+DX_OFFSET)
	LD B, (IY+REPS_OFFSET)
	
.loop1	
	LD A, (IY+TILENUM_OFFSET)	
	LD E, (IY+DY_OFFSET)	
	
	PUSH BC
	PUSH IY	
	call draw_tile
	POP IY
	POP BC
	LD A, D
	ADD C
	LD D, a		; We add the tile width

	LD A, B
	DEC A
	LD B, A
	JR NZ, .loop1	 

	LD A, D
	CP 255
	JR NZ, .next_element
	XOR a
	LD D, A

.next_element
[4]	INC IY		; TILEMAP_SIZE
	LD A, (IY)
	CP 255
	JR NZ, .map_element

	; Transparent Tiles
	LD IY, (stg_map_ptr_front)

.map_element_trans
	LD A, (IY+TILENUM_OFFSET)
	LD HL, metatiles_data
	ADD A,A
	ADD A,A ; A*4 (size of metatiles data)
	ADD A, METATILE_NX
	LD B, 0
	LD C, A
	ADD HL, BC
	
	LD C, (HL)	; Guardamos el ancho de la tile
	
	LD D, (IY+DX_OFFSET)
	LD B, (IY+REPS_OFFSET)
	
.loop1_trans
	LD A, (IY+TILENUM_OFFSET)	
	LD E, (IY+DY_OFFSET)	
	
	PUSH BC
	PUSH IY	
	call draw_tile_trans
	POP IY
	POP BC
	LD A, D
	ADD C
	LD D, a		; We add the tile width

	LD A, B
	DEC A
	LD B, A
	JR NZ, .loop1_trans 

	LD A, D
	CP 255
	JR NZ, .next_element_trans
	XOR a
	LD D, A

.next_element_trans
[4]	INC IY		; TILEMAP_SIZE
	LD A, (IY)
	CP 255
	JR NZ, .map_element_trans
	ret