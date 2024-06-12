stg2_map_back:
; Background Tiles - #Tile, X, Y, #reps
	db 9, 0, 0, 16    		
	db 9, 0, 16, 16
	db 9, 0, 32, 16	
	db 9, 0, 48, 16
	db 9, 0, 64, 16
	db 9, 0, 80, 16

	db 9, 0, 96, 7	
	db 7, 112, 96, 2
	db 9, 144, 96, 7

    db 9, 0, 112, 7	
	db 7, 112, 112, 2
	db 9, 144, 112, 7

    db 9, 0, 128, 7	
	db 7, 112, 128, 2
	db 9, 144, 128, 7

    db 9, 0, 144, 7	
	db 7, 112, 144, 2
	db 9, 144, 144, 7

    db 9, 0, 160, 7	
	db 7, 112, 160, 2
	db 9, 144, 160, 7

    db 9, 0, 176, 7	
	db 7, 112, 176, 2
	db 9, 144, 176, 7
	
	db 255		; fin
	
stg2_map_front:
	; Transparent Tiles
	db 10, 96, 160, 1	; Skull	

	db 11, 16, 112, 1	; Tree 1
	db 11, 224, 112, 1	; Tree 2
    db 11, 80, 144, 1	; Tree 3
	db 11, 160, 144, 1	; Tree 4

	db 15, 96, 16, 1	; Gargoyle
	db 16, 24, 8, 1	; Cross 1
    db 16, 184, 8, 1	; Cross 2
	db 255	; fin

