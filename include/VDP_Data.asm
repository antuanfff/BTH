; VDP COMMAND PARAMETER
;                               SXL SXH SYL SYH DXL DXH DYL DYH NXL NXH NYL NYH CLR ARG CMD
tileDatROM:                 db	0, 0, 0, TILES_PAGE, 0, 0, 0, FRONT_BUFFER, TILE_WIDTH, 0, TILE_HEIGHT, 0, 0, 0, CMD_HMMM
tileDatTransROM:			db	0, 0, 0, TILES_PAGE, 0, 0, 0, FRONT_BUFFER, TILE_WIDTH, 0, 1, 0, 0, 0, CMD_LMMM | VDP_TIMP;CMD_LMMM;CMD_HMMM
energyDatROM:               db	0, 0, 0, TILES_PAGE, 0, 0, 0, FRONT_BUFFER, ENERGY_WIDTH, 0, TILE_HEIGHT, 0, 0, 0, CMD_HMMM
DiagBoxToBackBufROM:        db	0, 0, 192, FRONT_BUFFER, 0, 0, 192, BACK_BUFFER, DIAGBOX_WIDTH, 0, DIAGBOX_HEIGHT, 0, 0, 0, CMD_HMMM 
DiagBoxToFrontkBufROM:      db	0, 0, 192, BACK_BUFFER, 0, 0, 192, FRONT_BUFFER, DIAGBOX_WIDTH, 0, DIAGBOX_HEIGHT, 0, 0, 0, CMD_HMMM 
DiagBoxClearROM:            db	0, 0, 192, TILES_PAGE, 0, 0, 192, FRONT_BUFFER, DIAGBOX_WIDTH, 0, DIAGBOX_HEIGHT, 0, 0, 0, CMD_HMMM 
