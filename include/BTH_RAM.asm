SHOWING_MIKE_DIALOG  DS 1
SHOWING_JOHN_DIALOG  DS 1
SHOWING_GUS_DIALOG  DS 1
FONT_HEIGHT EQU 8
FONT_WIDTH  EQU 8
CHR_ACR DS 2
PTR_STR_DX DS 1
PTR_STR_DY DS 1

LAST_KEY_PRESSED DS 1
OLD_KEY_PRESSED DS 1
JIFFY_TEMP DB $00
CHAR_SPEED_X DB $00
CHAR_SPEED_Y DB $00
CHAR_SPEED_X_GHOST DB $00
CHAR_SPEED_X_GHOST_STG2 DB $00
CHAR_GHOST_DEAD DS 1
CHAR_GHOST_DEAD_STG2 DS 1
CHAR_MIN_STEP_STG2 DS 1
CHAR_DIR_GHOST1 DB $00      ; $00 - LEFT, $FF - RIGHT
CHAR_DIR_GHOST_STG2 DB $00      ; $00 - LEFT, $FF - RIGHT
CHAR_DIR_MAIN DB $00        ; $00 - UP, $01 - DOWN, $02 - LEFT, $03 - RIGHT
CHAR_NEW_DIR_MAIN DS 1
CHAR_MAIN_SHOOT DS 1        ; $01 - SHOOT LEFT $02 - SHOOT RIGHT $03 - SHOOT UP $04 - SHOOT DOWN
CHAR_MIN_STEP DS 1          ; número de ciclos antes de cambiar de patrón
CHAR_DISTANCE_SHOOT DS 1
CHAR_SPEED_SHOOT DS 1
SPRITE_ATTRS:
    DS 128
SLOTBIOS:
    DS 1
SLOTGAME:
    DS 1
SLOTRAM:
    DS 1
SPRITE_PTR_REPLACE:
    DS 2
SPRITE_COLOR_REPLACE:
    DS 2
SPRITE_COLOR_REPLACE2:
    DS 2
MAPA:
    DS 2
BITMAP:
    DS 2