;counter
counter_stg1_solved_max equ #6F
counter_stg2_solved_max equ #6F

; pages for DATA

SPR_DATA_PAGE    equ 1
TILES_PAGE  equ 2

ENTITY_SIZE: EQU 9

; Sprite Collision

PLAYER_COLLISION_OFFSET_X      equ 15
PLAYER_COLLISION_OFFSET_Y      equ 16
PLAYER_WIDTH                   equ 14
PLAYER_HEIGHT                  equ 16

; Offset Entity struct
ENEMY_X                         equ 1
ENEMY_Y                         equ 2
ENTITY_ENERGY                   equ 3
ENEMY_COLLISION_OFFSET_X        equ 4
ENEMY_COLLISION_OFFSET_Y        equ 5
ENEMY_COLLISION_WIDTH           equ 6
ENEMY_COLLISION_HEIGHT          equ 7
SPRITENUMBER_SPAT               equ 8
damage                          equ 9

; Notes Black Sabbath stg2
STG2_TILE1_XH    equ 120
STG2_TILE1_XL    equ 112
STG2_TILE1_YH    equ 112
STG2_TILE1_YL    equ 96

STG2_TILE2_XH    equ STG2_TILE3_XH
STG2_TILE2_XL    equ STG2_TILE3_XL
STG2_TILE2_YH    equ STG2_TILE1_YH
STG2_TILE2_YL    equ STG2_TILE1_YL

STG2_TILE3_XH    equ 129
STG2_TILE3_XL    equ 121
STG2_TILE3_YH    equ 144
STG2_TILE3_YL    equ 128

STG2_MURRAY_X     equ 113
STG2_MURRAY_XL    equ 112
STG2_MURRAY_YH    equ 172
STG2_MURRAY_YL    equ 156

; VDP Const
;IRST_LINE_DLG_BOX   equ 5C28H
;SEC_LINE_DLG_BOX   equ 6028H
;THIRD_LINE_DLG_BOX   equ 6428H

; Energy             lv1, lv2, lv3, lv4, lv5, lv6
ANDY_MAX_ENERGY:  db  24, 32, 40, 48, 56, 64

FIRST_LINE_DLG_BOX   equ 6100H
SEC_LINE_DLG_BOX   equ 6500H
THIRD_LINE_DLG_BOX   equ 6900H

FIRST_LINE_DLG_BOX_v2   equ 194
SEC_LINE_DLG_BOX_v2   equ 202

ROMMODE                 equ 1 ; 0 Dos Mode 1 Rom Mode
KB_RIGHT 				equ 7
KB_DOWN 				equ 6
KB_UP 					equ	5        
KB_LEFT 				equ	4
KB_DEL 					equ	3
KB_SPACE				equ	0

MOV_SPEED				equ 1
MOV_SPEED_GHOST			equ 1
MOV_SPEED_SHOOT         equ 3
MAX_DISTANCE_SHOOT      equ 48
MAX_CHAR_STEPS          equ 5       ; #steps pattern change MAIN CHAR
MAX_CHAR_STEPS_STG2          equ 6
REG9SAV                 equ #FFE8
PORT_0                  equ 0x98
PORT_1                  equ 0x99
PORT_2                  equ 0x9a
PORT_3                  equ 0x9b

RG15AV  equ #F3E0 ; alamcena el valor del registro 1 de escritura del VDP, hay unas rutinas de la bios que guardan estas direcciones valores globals del sistema
FORCLR  equ $F3E9 ; Foreground colour
BAKCLR  equ $f3ea ; Background colour
BDRCLR  equ $f3eb ; Border colour

MIKE_TOMB_STG1_X            equ 200
JOHN_TOMB_STG1_X            equ 41
GUS_TOMB_STG1_X             equ 73
GUS_TOMB_STG1_Y             equ 120
SKULL_TOMB_STG1_X             equ 105
SKULL_TOMB_STG1_Y1             equ 90
SKULL_TOMB_STG1_Y2             equ 100

SPR_SHOOT_P1                equ 12  ; Offset de IX (SPRATT)
SPR_SHOOT_P1_PTRN           equ 18h
SPR_GHOST_STG1              equ 16  ; Offset de IX (SPRATT)
SPR_GHOST_STG1_PTRN_L1      equ 1Ch  ; Offset del Sprite Pattern Table
SPR_GHOST_STG1_PTRN_L2      equ 24h  ; Offset del Sprite Pattern Table
SPR_GHOST_STG1_PTRN_R1      equ 2Ch  ; Offset del Sprite Pattern Table
SPR_GHOST_STG1_PTRN_R2      equ 34h  ; Offset del Sprite Pattern Table

SPR_GHOST_STG2              equ 24  ; Offset de IX (SPRATT)
SPR_GHOST_STG2_PTRN_L1      equ 3Ch  ; Offset del Sprite Pattern Table
SPR_GHOST_STG2_PTRN_L2      equ 44h  ; Offset del Sprite Pattern Table
SPR_GHOST_STG2_PTRN_R1      equ 4Ch  ; Offset del Sprite Pattern Table
SPR_GHOST_STG2_PTRN_R2      equ 54h  ; Offset del Sprite Pattern Table

