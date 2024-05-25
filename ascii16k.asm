; Example to create an MegaRom of 128kB that use an ASCII 16K Mapper
; for Sjasm assembler

	output ASC16tst.ROM

LF:	equ	0Ah
CR:	equ	0Dh

ENASLT:	equ	0024h
INIT32:	equ	006Fh
CHPUT:	equ	00A2h	; Address of character output routine of main Rom BIOS
RSLREG:	equ	0138h

PageSize:	equ	04000h	; 16kB
Seg_P8000_SW:	equ	07000h	; Segment switch for page 8000h-BFFFh (ASCII 16k Mapper)

LINL32:	equ	0F3AFh
EXPTBL:	equ	0FCC1h		; Extended slot flags table (4 bytes)


	defpage 0,4000H,PageSize
	page 0

	db	41h,42h
	dw	INIT,0,0,0,0,0,0

INIT:
	ld	a,32
	ld	(LINL32),a	; 32 columns
	call	INIT32		; SCREEN 1

; Typical routine to select the ROM on page 8000h-BFFFh from page 4000h-7BFFFh

	call	RSLREG
	rrca
	rrca
	and	3	;Keep bits corresponding to the page 4000h-7FFFh
	ld	c,a
	ld	b,0
	ld	hl,EXPTBL
	add	hl,bc
	ld	a,(hl)
	and	80h
	or	c
	ld	c,a
	inc	hl
	inc	hl
	inc	hl
	inc	hl
	ld	a,(hl)
	and	0Ch
	or	c
	ld	h,080h
	call	ENASLT		; Select the ROM on page 8000h-BFFFh

	ld	a,3
LOOP:
	ld	(Seg_P8000_SW),a	; Select the segment on page 8000h-BFFFh

	push	af
	ld	hl,Seg1_TXT	; Text pointer into HL
	call	Print		; Call the routine Print below
	pop	af

	inc	a	; Increment segment number
	cp	8
	jr	nz, LOOP	; Jump to LOOP if A<8
 
Finished:
	jr	Finished	; Jump to itself endlessly.

Print:
	ld	a,(hl)		; Load the byte from memory at address indicated by HL to A.
	and	a		; Same as CP 0 but faster.
	ret	z		; Back behind the call print if A = 0
	call	CHPUT		; Call the routine to display a character.
	inc	hl		; Increment the HL value.
	jr	Print		; Jump to the address in the label Print.
 
	defpage 1,8000H,PageSize
	page 1

Seg1_TXT:			; Text pointer label
	db "Text from segment 1",LF,CR,0	; Zero indicates the end of text.

	defpage 2,8000H,PageSize
	page 2

	db "Text from segment 2",LF,CR,0

	defpage 3,8000H,PageSize
	page 3

	db "Text from segment 3",LF,CR,0

	defpage 4,8000H,PageSize
	page 4

	db "Text from segment 4",LF,CR,0

	defpage 5,8000H,PageSize
	page 5

	db "Text from segment 5",LF,CR,0

	defpage 6,8000H,PageSize
	page 6

	db "Text from segment 6",LF,CR,0

	defpage 7,8000H,PageSize
	page 7

	db "Text from segment 7",LF,CR,0