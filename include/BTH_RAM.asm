JIFFY_TEMP DB $00
CHAR_SPEED_X DB $00
CHAR_SPEED_Y DB $00
CHAR_SPEED_X_GHOST DB $00
CHAR_GHOST_DEAD DS 1
CHAR_DIR_GHOST1 DB $00      ; $00 - LEFT, $FF - RIGHT
CHAR_DIR_MAIN DB $00        ; $00 - UP, $01 - DOWN, $02 - LEFT, $03 - RIGHT
CHAR_NEW_DIR_MAIN DS 1
CHAR_MAIN_SHOOT DS 1        ; $01 - SHOOT LEFT $02 - SHOOT RIGHT
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
BITMAP:
    DS 2