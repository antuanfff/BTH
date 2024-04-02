; VDP Const
FIRST_LINE_DLG_BOX   equ 5C28H
SEC_LINE_DLG_BOX   equ 6028H
THIRD_LINE_DLG_BOX   equ 6428H

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
MAX_CHAR_STEPS          equ 4       ; #steps pattern change MAIN CHAR
MAX_CHAR_STEPS_STG2          equ 6
REG9SAV                 equ #FFE8
PORT_0                  equ 0x98
PORT_1                  equ 0x99
PORT_2                  equ 0x9a
PORT_3                  equ 0x9b

RG15AV equ #F3E0 ; alamcena el valor del registro 1 de escritura del VDP, hay unas rutinas de la bios que guardan es entas direcciones valores globals del sistema
FORCLR equ $F3E9 ; Foreground colour

