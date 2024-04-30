		output	"main.rom"; acuerdate de poner los espacios delante de output
		
		org		#4000
			
		;Cabecera
		db "AB"
		word INICIO
		word 0,0,0,0,0,0				


INICIO:
    call Inicializar_variables

    call Actualizar_volumen_canal_a
    call Actualizar_tono_canal_a
    call Capturar_teclas

    
Inicializar_variables:
	;les ponemos a todas las variables el valor de #03
	ld a,#06
    ld (#f000),a ;En la direccion #f000 de la RAM estará el volumen
	ld a,#03
    ld (#f001),a ;En #f001 estará el tono
    ret
Actualizar_volumen_canal_a:
	ld	a, #08 ;Le decimos que queremos configurar el regostro 8 
	out (#A0), a
	ld a, (#f000) ;le ponemos el valor inicial de volumen 3
	out (#A1),a
	ret
Actualizar_tono_canal_a:
	ld a,#01; el regisro que qeuremos modificar para la frecuencia es el 1
	out (#A0), a
	ld a,(#f001) ;le ponemos el valor inicial de tono de 3
	out (#A1),a 
	ret



;********************Este loop estará siempre comprobando las teclas********************
Capturar_teclas:
    call Escribir_pulsa_una_tecla
    call #009F ; chget pone en el registro a del z80 el caracter ascii que has escrito en el teclado
    call #00A2 ;presenta en pantalla el caracter almacenado en el registro a del z80
    cp 'q'
    jp z, rutinas_pulsacion_subir_volumen
    cp 'a'
    jp z, rutinas_pulsacion_bajar_volumen
    cp 'o'
    jp z, rutinas_pulsacion_bajar_tono
    cp 'p'
    jp z, rutinas_pulsacion_subir_tono
    jp nz, Escribir_tecla_no_valida

    jr Capturar_teclas
;********************Final de la comprobación de teclas*****************************

rutinas_pulsacion_subir_volumen:
    call Borrar_pantalla
    ld hl, Texto_has_pulsado_subir_volumen ; para imprimir un texto en la pantalla
    call Imprimir
    call Subir_variable_volumen
    call Mostrar_valor_variable_volumen
    call Capturar_teclas
rutinas_pulsacion_bajar_volumen:
    call Borrar_pantalla
    ld hl, Texto_has_pulsado_bajar_volumen
    call Imprimir
    call Diminuir_variable_volumen
    call Mostrar_valor_variable_volumen
    call Capturar_teclas
rutinas_pulsacion_subir_tono:
    call Borrar_pantalla
    ld hl, Texto_has_pulsado_subir_tono
    call Imprimir
    call Subir_variable_tono
    call Mostrar_valor_variable_tono
    call Capturar_teclas
rutinas_pulsacion_bajar_tono:
    call Borrar_pantalla
    ld hl, Texto_has_pulsado_bajar_tono
    call Imprimir
    call Diminuir_variable_tono
    call Mostrar_valor_variable_tono
    call Capturar_teclas






Subir_variable_volumen:
    ld a, (#f000)
    add 1
    ld (#f000),a
    call Actualizar_volumen_canal_a
    ret
Diminuir_variable_volumen:
    ld a,(#f000)
    sub 1    
    ld (#f000),a
    call Actualizar_volumen_canal_a
    ret
Subir_variable_tono:
    ld a, (#f001)
    add 1
    ld (#f001),a
    call Actualizar_tono_canal_a
    ret
Diminuir_variable_tono:
    ld a,(#f001)
    sub 1
    ld (#f001),a
    call Actualizar_tono_canal_a
    ret



Escribir_tecla_no_valida
    call #00A2
    ld hl, Texto_tecla_no_valida
    call Imprimir
    call Capturar_teclas

Borrar_pantalla:
    XOR a
    call #00C3;borramos la pantalla
    ret
Mostrar_valor_variable_volumen:
    ld a,(#f000)
    add #30
    call #00A2
    ret
Mostrar_valor_variable_tono:
    ld a,(#f001)
    add #30
    call #00A2
    ret

Imprimir:
    ld  a,(hl)           
    cp 0            
    ret z  
    call #00A2        
    inc hl             
    jr Imprimir 

Escribir_pulsa_una_tecla:
    ld hl, Pulsa_una_tecla
    call Imprimir
    ret
Escribir_texto_final_del_programa:
    ld hl, Texto_final_del_programa
    call Imprimir
    ret


Pulsa_una_tecla:
    db "\n\nPulsa 'q' para subir volumen 'a' para bajar volumen, 'o' para bajar tono y 'q' para subir tono. ",0
Texto_has_pulsado_subir_volumen:
    db "Has pulsado subir volumen. ",0
Texto_has_pulsado_bajar_volumen:
    db "Has pulsado bajar volumen. ",0
Texto_has_pulsado_subir_tono:
    db "Has pulsado subir tono. ",0
Texto_has_pulsado_bajar_tono:
    db "Has pulsado bajar tono. ",0
Texto_tecla_no_valida:
    db "\nTecla no valida 'q' para subir volumen 'a' para bajar volumen, 'o' para bajar tono y 'q' para subir tono. ",0
Texto_final_del_programa:
    db "Final del programaa. ",0


relleno_de_bytes:
		ds	#8000-$	

