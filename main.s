
		output "main.rom"
romheader:
	org		#4000
	db "AB"
	word START
	word 0,0,0,0,0,0				
		
START:
	di	
	ld		hl,SONG-99		; hl vale la direccion donde se encuentra la cancion - 99
	call	PT3_INIT			; Inicia el reproductor de PT3
	ei
	;*************Poner la logica del programa desde aqui*****************/
	call mostrar_texto_en_pantalla
	;***********************Fin de la lógica del juego********************/
	
loop:
	halt						;sincronizacion
	di
	call	PT3_ROUT			;envia datos a al PSG
	call	PT3_PLAY			;prepara el siguiente trocito de cancion que sera enviada mas tarde al PSG
	ei
	jp		loop


;*************Poner la logica del programa desde aqui*****************/
mostrar_texto_en_pantalla:
	ld hl,mensaje       
    call Imprimir_mensaje_cacnion                    

Imprimir_mensaje_cacnion:
    ld  a,(hl)         
    and a              
    ret z              
    call #00A2         
    inc hl             
    jr Imprimir_mensaje_cacnion          
;***********************Fin de la lógica del juego********************/

	include	"include\PT3_player.s"					;replayer de PT3
SONG:
	incbin "musica_sin_cabacera.pt3"			;musica de ejemplo
mensaje: 
    db "Cancio Catalunya Lliure! ",0	
relleno_de_bytes:
	ds #8000-$		




END


