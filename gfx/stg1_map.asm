stg1_map_back:
; Compressed: #Tile, X, Y, #reps
	db 9, 0, 0, 7
	db 0, 112, 0, 1
	db 9, 128, 0, 7

	db 9, 0, 16, 7
	db 7, 112, 16, 2
	db 9, 128, 16, 7
	db #55
	
stg1_map_front:
	; Transparent Tiles
	db 10, 96, 96, 1	; Skull
	db 13, 32, 32, 1	; RIP
	db 12, 64, 128, 1	; Antichrist
	db 13, 208, 96, 1	; Christ

	db 11, 158, 52, 1	; Tree 1
	db 11, 172, 52, 1	; Tree 1
	db #55

