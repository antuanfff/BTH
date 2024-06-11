stg1_map_back:
; Background Tiles - #Tile, X, Y, #reps
	db 9, 0, 0, 7	
	db 0, 112, 0, 1	
	db 9, 144, 0, 7	

	db 9, 0, 16, 7
	db 7, 112, 16, 2
	db 9, 144, 16, 7

	db 9, 0, 32, 3
	db 7, 48, 32, 6
	db 9, 144, 32, 7

	db 9, 0, 48, 7
	db 7, 112, 48, 2
	db 9, 144, 48, 7

	db 9, 0, 64, 7
	db 7, 112, 64, 2
	db 9, 144, 64, 7

	db 9, 0, 80, 7
	db 7, 112, 80, 2
	db 9, 144, 80, 7

	db 9, 0, 96, 7
	db 7, 112, 96, 6
	db 9, 208, 96, 3

	db 9, 0, 112, 7
	db 7, 112, 112, 2
	db 9, 144, 112, 7

	db 9, 0, 128, 5
	db 7, 80, 128, 4
	db 9, 144, 128, 7

	db 9, 0, 144, 7
	db 7, 112, 144, 2
	db 9, 144, 144, 7

	db 9, 0, 160, 7
	db 7, 112, 160, 2
	db 9, 144, 160, 7

	db 9, 0, 176, 16

	db 255		; fin
	
stg1_map_front:
	; Transparent Tiles
	db 10, 96, 96, 1	; Skull
	db 13, 32, 32, 1	; RIP
	db 12, 64, 128, 1	; Antichrist
	db 14, 208, 96, 1	; Christ

	db 11, 158, 52, 1	; Tree 1
	db 11, 172, 52, 1	; Tree 1
	db 255	; fin

