metatiles_data:
    ; SX, SY, NX, NY, Free
    db 0, 0, 32, 16, 0  ; gate lockpad
    db 32, 0, 32, 16, 0  ; gate closed
    db 64, 0, 32, 16, 0  ; gate half opened
    db 96, 0, 32, 16, 0  ; gate opened

    db 128, 0, 16, 16, 0  ; blood drop full
    db 144, 0, 16, 16, 0  ; blood drop half
    db 160, 0, 16, 16, 0  ; blood drop empty

    db 176, 0, 16, 16, 0  ; Tile
    db 192, 0, 16, 16, 0  ; Pressed tile

    db 208, 0, 16, 16, 0  ; Gress
    db 224, 0, 16, 16, 0  ; Skull
    db 240, 0, 16, 16, 0  ; Tree