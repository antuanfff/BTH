metatiles_data:
    ; SX, SY, NX, NY
    db 0, 0, 32, 16   ; 0 - gate lockpad
    db 32, 0, 32, 16  ; 1 - gate closed
    db 64, 0, 32, 16  ; 2 - gate half opened
    db 96, 0, 32, 16  ; 3 - gate opened

    db 128, 0, 16, 16  ; 4 - blood drop full
    db 144, 0, 16, 16  ; 5 - blood drop half
    db 160, 0, 16, 16  ; 6 - blood drop empty

    db 176, 0, 16, 16  ; 7 - Tile
    db 192, 0, 16, 16  ; 8 - Pressed tile

    db 208, 0, 16, 16  ; 9 - Gress
    db 224, 0, 16, 16  ; 10 - Skull
    db 240, 0, 16, 16  ; 11 - Tree