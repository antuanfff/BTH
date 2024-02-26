CHGET                   equ #009F    ; Se queda esperando que se pulse una tecla
CHPUT                   equ #00A2    ; escribe el caracter ascii almacenado en a                         
CHGMOD                  equ #005F   ; Cambia el modo de screen pero previamente necesita que se le asigne el modo en el registro a
LDIRVM                  equ #005C   ;Tansfiere bloques de la RAM a la VRAM, es la más importante, necesita previamente asignar valor al registro bc con la longitud, dc con la dirección de inicio de la VRAM y hl con la dirección de inicio de la RAM:
RDVDP                   equ #013E    ; Lee el registro de estado del VDP
WRTVDP                  equ #0047   ; Escribe en los registros del VDP 
CLIKSW                  equ $F3DB ; Keyboard click sound
SNSMAT					equ #0141
JIFFY 					equ #FC9E
DISSCR                  equ #0041   ; Disable screen
ENASCR                  equ #0044   ; Enable screen
INIGRP                  equ #0072   ; Enable Screen 2
RSLREG                  equ #0138   ; Leemos el contenido del registro de seleccion de slots
ENASLT                  equ #0024   ; Switches indicated slot at indicated page on perpetually Input    : A - Slot ID, see RDSLT            H - Bit 6 and 7 must contain the page number (00-11)