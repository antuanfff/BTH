SPRITE_P1_UP:
    ; UP 1        
    ; mask 0
    DB $03,$0F,$1F,$1F,$1F,$3F,$1F,$0F
    DB $30,$78,$60,$60,$03,$03,$06,$0E
    DB $E0,$F0,$F8,$F8,$FC,$F8,$FC,$F0
    DB $0E,$18,$06,$00,$C0,$E0,$F0,$00    
    ; mask 1
    DB $00,$00,$00,$00,$00,$00,$00,$00
    DB $0F,$00,$03,$03,$01,$01,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00
    DB $F0,$06,$C0,$C0,$80,$80,$80,$00
    ; mask 2
    DB $00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$07,$01,$01,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$E0,$80,$80,$00,$00,$00,$00    

    ; UP 2
    ; mask 0
    DB $03,$0F,$1F,$1F,$1F,$3F,$1F,$0F
    DB $70,$18,$60,$00,$03,$07,$0E,$00
    DB $E0,$F0,$F8,$F8,$FC,$F8,$FC,$F0
    DB $0C,$1E,$06,$06,$C0,$C0,$E0,$70    
    ; mask 1
    DB $00,$00,$00,$00,$00,$00,$00,$00
    DB $0F,$60,$03,$03,$01,$01,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00
    DB $F0,$00,$C0,$C0,$80,$80,$80,$00
    ; mask 2
    DB $00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$07,$01,$01,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$E0,$80,$80,$00,$00,$00,$00

SPRITE_P1_DOWN:
    ; DOWN 1 - 00h
    DB $03,$0F,$00,$0A,$0E,$0D,$07,$03
    DB $3E,$7B,$60,$60,$03,$03,$06,$0E
    DB $E0,$F0,$A0,$90,$70,$B0,$E0,$C0
    DB $7E,$D8,$06,$00,$C0,$E0,$F0,$00
    ; mask 1 - 04h
    DB $00,$00,$1F,$15,$11,$32,$18,$0C
    DB $01,$00,$03,$03,$01,$01,$00,$00
    DB $00,$00,$58,$68,$8C,$48,$1C,$30
    DB $80,$06,$C0,$C0,$80,$80,$80,$00
    ; mask 2 - 08h
    DB $00,$00,$00,$00,$00,$30,$00,$00
    DB $00,$04,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$08,$00,$00
    DB $00,$20,$00,$00,$00,$00,$00,$00

    ; DOWN 2
    ; mask 0 - 0Ch
    DB $03,$0F,$00,$0A,$0E,$0D,$07,$03
    DB $7E,$1B,$60,$00,$03,$07,$0E,$00
    DB $E0,$F0,$A0,$90,$70,$B0,$E0,$C0
    DB $7C,$DE,$06,$06,$C0,$C0,$E0,$70
    ; mask 1 - 10h
    DB $00,$00,$1F,$15,$11,$32,$18,$0C
    DB $01,$60,$03,$03,$01,$01,$00,$00
    DB $00,$00,$58,$68,$8C,$48,$1C,$30
    DB $80,$00,$C0,$C0,$80,$80,$80,$00
    ; mask 2 - 14h
    DB $00,$00,$00,$00,$00,$30,$00,$00
    DB $00,$04,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$08,$00,$00
    DB $00,$20,$00,$00,$00,$00,$00,$00

SPRITE_P1_LEFT:         
    ; --- Slot 1
    ; mask 0
    DB $07,$1F,$03,$07,$0B,$1F,$0F,$07
    DB $03,$0F,$30,$30,$03,$01,$01,$03
    DB $C0,$E0,$00,$80,$C0,$C0,$80,$00
    DB $C0,$80,$00,$00,$D8,$B0,$20,$00
    ; mask 1
    DB $00,$00,$0C,$18,$34,$00,$00,$00
    DB $0C,$00,$09,$03,$00,$00,$00,$00
    DB $00,$00,$F0,$78,$38,$3C,$70,$F8
    DB $00,$78,$C0,$F8,$18,$10,$00,$00
    ; mask 2
    DB $00,$00,$00,$00,$30,$00,$00,$00
    DB $00,$00,$01,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$38,$00,$00,$00
    DB $3C,$00,$F0,$38,$00,$00,$00,$00
        
    ; --- Slot 3
    ; mask 0
    DB $07,$1F,$03,$07,$0B,$1F,$0F,$07
    DB $03,$0F,$01,$03,$03,$01,$03,$06
    DB $C0,$E0,$00,$80,$C0,$C0,$80,$00
    DB $C0,$80,$F0,$F8,$D8,$D0,$40,$C0
    ; mask 1
    DB $00,$00,$0C,$18,$34,$00,$00,$00
    DB $0C,$30,$31,$00,$00,$00,$00,$00
    DB $00,$00,$F0,$78,$38,$3C,$70,$F8
    DB $00,$00,$C0,$38,$18,$10,$00,$00
    ; mask 2
    DB $00,$00,$00,$00,$30,$00,$00,$00
    DB $00,$00,$30,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$38,$00,$00,$00
    DB $3C,$78,$00,$00,$00,$00,$00,$00

SPRITE_P1_RIGHT:
    ; --- Slot 0
    ; mask 0
    DB $03,$07,$00,$01,$03,$03,$01,$00
    DB $03,$01,$00,$00,$1B,$0D,$04,$00
    DB $E0,$F8,$C0,$E0,$D0,$F8,$F0,$E0
    DB $C0,$F0,$0C,$0C,$C0,$80,$80,$C0
    ; mask 1
    DB $00,$00,$0F,$1E,$1C,$3C,$0E,$1F
    DB $00,$1E,$03,$1F,$18,$08,$00,$00
    DB $00,$00,$30,$18,$2C,$00,$00,$00
    DB $30,$00,$90,$C0,$00,$00,$00,$00
    ; mask 2
    DB $00,$00,$00,$00,$1C,$00,$00,$00
    DB $3C,$00,$0F,$1C,$00,$00,$00,$00
    DB $00,$00,$00,$00,$0C,$00,$00,$00
    DB $00,$00,$80,$00,$00,$00,$00,$00

    ; --- Slot 2
    ; mask 0
    DB $03,$07,$00,$01,$03,$03,$01,$00
    DB $03,$01,$0F,$1F,$1B,$0B,$02,$03
    DB $E0,$F8,$C0,$E0,$D0,$F8,$F0,$E0
    DB $C0,$F0,$80,$C0,$C0,$80,$C0,$60
    ; mask 1
    DB $00,$00,$0F,$1E,$1C,$3C,$0E,$1F
    DB $00,$00,$03,$1C,$18,$08,$00,$00
    DB $00,$00,$30,$18,$2C,$00,$00,$00
    DB $30,$0C,$8C,$00,$00,$00,$00,$00
    ; mask 2
    DB $00,$00,$00,$00,$1C,$00,$00,$00
    DB $3C,$1E,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$0C,$00,$00,$00
    DB $00,$00,$0C,$00,$00,$00,$00,$00

SPRITE_PATTERN:    
    ; DOWN 1 
    ; mask 0 - 00h
    DB $03,$0F,$00,$0A,$0E,$0D,$07,$03
    DB $3E,$7B,$60,$60,$03,$03,$06,$0E
    DB $E0,$F0,$A0,$90,$70,$B0,$E0,$C0
    DB $7E,$D8,$06,$00,$C0,$E0,$F0,$00
    ; mask 1 - 04h
    DB $00,$00,$1F,$15,$11,$32,$18,$0C
    DB $01,$00,$03,$03,$01,$01,$00,$00
    DB $00,$00,$58,$68,$8C,$48,$1C,$30
    DB $80,$06,$C0,$C0,$80,$80,$80,$00
    ; mask 2 - 08h
    DB $00,$00,$00,$00,$00,$30,$00,$00
    DB $00,$04,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$08,$00,$00
    DB $00,$20,$00,$00,$00,$00,$00,$00

    ; DOWN 2 
    ; mask 0 - 0Ch
    DB $03,$0F,$00,$0A,$0E,$0D,$07,$03
    DB $7E,$1B,$60,$00,$03,$07,$0E,$00
    DB $E0,$F0,$A0,$90,$70,$B0,$E0,$C0
    DB $7C,$DE,$06,$06,$C0,$C0,$E0,$70
    ; mask 1 - 10h
    DB $00,$00,$1F,$15,$11,$32,$18,$0C
    DB $01,$60,$03,$03,$01,$01,$00,$00
    DB $00,$00,$58,$68,$8C,$48,$1C,$30
    DB $80,$00,$C0,$C0,$80,$80,$80,$00
    ; mask 2 - 14h
    DB $00,$00,$00,$00,$00,$30,$00,$00
    DB $00,$04,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$08,$00,$00
    DB $00,$20,$00,$00,$00,$00,$00,$00
    
    ;Ghost 1 Left - 18H
    DB 00h,01h,07h,0fh,1fh,3fh,7fh,01h
    DB $15,$14,$7f,$3f,$3f,$3f,$3f,$7f
    DB $00,$e0,$80,$00,$00,$80,$80,$c0
    DB $c0,$e0,$f0,$f8,$fc,$fc,$fe,$ff  
    
    ; Ghost 2 Left - 1Ch
    DB $00,$01,$07,$0f,$1f,$3f,$7f,$01
    DB $15,$14,$7f,$3f,$3f,$7f,$7f,$83
    DB $00,$e0,$80,$00,$00,$80,$80,$c0
    DB $c0,$e0,$e0,$f0,$f8,$fc,$fe,$ff      

    ; Ghost 1 Right - 20h
    DB $00,$07,$01,$00,$00,$01,$01,$03
    DB $03,$07,$07,$0f,$1f,$3f,$7f,$ff
    DB $00,$80,$e0,$f0,$f8,$fc,$fe,$80
    DB $a8,$28,$fe,$fc,$fc,$fe,$fe,$c1
    
    ; Ghost 2 Right - 24h
    DB $00,$07,$01,$00,$00,$01,$01,$03
    DB $03,$07,$0f,$1f,$3f,$3f,$7f,$ff
    DB $00,$80,$e0,$f0,$f8,$fc,$fe,$80
    DB $a8,$28,$fe,$fc,$fc,$fc,$fc,$fe  

    ; Antichrist - RIGHT
    ; mask 0 - 28h
    ;DB $00,$00,$00,$03,$07,$0F,$1E,$1E
    ;DB $1E,$1C,$1E,$0F,$07,$03,$00,$00
    ;DB $00,$00,$00,$00,$80,$C0,$E0,$F0
    ;DB $F8,$70,$E0,$C0,$80,$00,$00,$00
    DB $00,$00,$00,$01,$01,$01,$01,$01
    DB $01,$01,$07,$07,$01,$01,$00,$00
    DB $00,$00,$00,$80,$80,$80,$80,$80
    DB $80,$80,$E0,$E0,$80,$80,$00,$00


    ; Esqueleto
    ; mask 0 - 2Ch
        ; --- Slot 0    
    DB $01,$03,$03,$03,$01,$00,$00,$01
    DB $07,$0B,$13,$0B,$03,$04,$04,$0C
    DB $F8,$F8,$74,$24,$FC,$F8,$F8,$00
    DB $C0,$A0,$90,$20,$80,$40,$40,$60    
    ; mask 1 - 30h
    DB $00,$01,$01,$01,$00,$00,$00,$00
    DB $00,$08,$11,$09,$01,$00,$00,$00
    DB $F8,$F8,$FC,$FC,$FC,$78,$50,$00
    DB $00,$A0,$90,$20,$80,$00,$00,$00    

SPRITE_COLOR_TABLE:       
     ;DOWN 1
    ; attr 0
    DB $06,$06,$0B,$0B,$0B,$0B,$0B,$0B
    DB $0C,$0C,$0B,$0B,$04,$04,$04,$04
    ; attr 1
    DB $00,$00,$46,$46,$46,$44,$46,$46
    DB $4B,$4B,$44,$44,$42,$42,$42,$00
    ; attr 2
    DB $00,$00,$00,$00,$00,$42,$00,$00
    DB $00,$46,$00,$00,$00,$00,$00,$00
         
    ; Cada posición ha de corresponder con la tabla de atributos    

    ; Fantasma
    DB $08,$08,$08,$08,$08,$08,$08,$08
    DB $08,$08,$08,$08,$08,$08,$08,$08
    ; Fantasma
    ;DB $08,$08,$08,$08,$08,$08,$08,$08
    ;DB $08,$08,$08,$08,$08,$08,$08,$08
    ; Fantasma
    ;DB $08,$08,$08,$08,$08,$08,$08,$08
    ;DB $08,$08,$08,$08,$08,$08,$08,$08
    ; Fantasma
    ;DB $08,$08,$08,$08,$08,$08,$08,$08
    ;DB $08,$08,$08,$08,$08,$08,$08,$08    
    ; Me ahorro los colores del fantasma, la 4a posicion del array de atributos será para el disparo
    ; Boomerang
    ;DB $08,$08,$08,$08,$08,$08,$08,$08
    ;DB $08,$08,$08,$08,$08,$08,$08,$08
    DB $00,$00,$00,$0F,$0F,$0F,$0F,$0F
    DB $0F,$0F,$0F,$0F,$0F,$0F,$00,$00
    ; Esqueleto 1
    ; attr 0
    DB $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    DB $0F,$0E,$0E,$0E,$0E,$0F,$0F,$0F
    ; Esqueleto 2
    ; attr 1
    DB $41,$41,$49,$49,$41,$41,$41,$00
    DB $00,$41,$41,$41,$41,$00,$00,$00

SPRITE_COLOR_P1_UP:
    ;UP 1
    ; attr 0
    DB $06,$06,$06,$06,$06,$06,$06,$06
    DB $0C,$0C,$0B,$0B,$04,$04,$04,$04
    ; attr 1
    DB $00,$00,$00,$00,$00,$00,$00,$00
    DB $46,$4B,$44,$44,$42,$42,$42,$00
    ; attr 2
    DB $00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$46,$42,$42,$00,$00,$00,$00   
    
    ;SPRITE_COLOR_P1_UP2:
    ;UP 1
    ; attr 0
    DB $06,$06,$06,$06,$06,$06,$06,$06
    DB $0C,$0C,$0B,$0B,$04,$04,$04,$04
    ; attr 1
    DB $00,$00,$00,$00,$00,$00,$00,$00
    DB $46,$4B,$44,$44,$42,$42,$42,$00
    ; attr 2
    DB $00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$46,$42,$42,$00,$00,$00,$00   

SPRITE_COLOR_P1_DOWN:
    ;DOWN 1
    ; attr 0
    DB $06,$06,$0B,$0B,$0B,$0B,$0B,$0B
    DB $0C,$0C,$0B,$0B,$04,$04,$04,$04
    ; attr 1
    DB $00,$00,$46,$46,$46,$44,$46,$46
    DB $4B,$4B,$44,$44,$42,$42,$42,$00
    ; attr 2
    DB $00,$00,$00,$00,$00,$42,$00,$00
    DB $00,$46,$00,$00,$00,$00,$00,$00        

    ;SPRITE_COLOR_P1_DOWN2:
    ;DOWN 1
    ; attr 0
    DB $06,$06,$0B,$0B,$0B,$0B,$0B,$0B
    DB $0C,$0C,$0B,$0B,$04,$04,$04,$04
    ; attr 1
    DB $00,$00,$46,$46,$46,$44,$46,$46
    DB $4B,$4B,$44,$44,$42,$42,$42,$00
    ; attr 2
    DB $00,$00,$00,$00,$00,$42,$00,$00
    DB $00,$46,$00,$00,$00,$00,$00,$00        

SPRITE_COLOR_P1_RIGHT:
    ; attr 0
    DB $06,$06,$0B,$0B,$0B,$0B,$0B,$0B
    DB $0C,$0C,$0B,$0B,$04,$04,$04,$04
    ; attr 1
    DB $00,$00,$46,$46,$44,$46,$46,$46
    DB $4B,$46,$4C,$44,$42,$42,$00,$00
    ; attr 2
    DB $00,$00,$00,$00,$42,$00,$00,$00
    DB $46,$00,$46,$42,$00,$00,$00,$00
    ; attr 0
    DB $06,$06,$0B,$0B,$0B,$0B,$0B,$0B
    DB $0C,$0C,$06,$04,$04,$04,$04,$04
    ; attr 1
    DB $00,$00,$46,$46,$44,$46,$46,$46
    DB $4B,$4B,$4A,$42,$42,$42,$00,$00
    ; attr 2
    DB $00,$00,$00,$00,$42,$00,$00,$00
    DB $46,$46,$41,$00,$00,$00,$00,$00

mapa:      
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        

      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
    
mapa0:      
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#00,#00,#00,#00,#00,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01       
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        

      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01        
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
    

SPRITE_COLOR_P1_LEFT:
    ; attr 0
    DB $06,$06,$0B,$0B,$0B,$0B,$0B,$0B
    DB $0C,$0C,$0B,$0B,$04,$04,$04,$04
    ; attr 1
    DB $00,$00,$46,$46,$44,$46,$46,$46
    DB $4B,$46,$4C,$44,$42,$42,$00,$00
    ; attr 2
    DB $00,$00,$00,$00,$42,$00,$00,$00
    DB $46,$00,$46,$42,$00,$00,$00,$00
    
    ;SPRITE_COLOR_P1_LEFT2:
    ; attr 0
    DB $06,$06,$0B,$0B,$0B,$0B,$0B,$0B
    DB $0C,$0C,$06,$04,$04,$04,$04,$04
    ; attr 1
    DB $00,$00,$46,$46,$44,$46,$46,$46
    DB $4B,$4B,$4A,$42,$42,$42,$00,$00
    ; attr 2
    DB $00,$00,$00,$00,$42,$00,$00,$00
    DB $46,$46,$41,$00,$00,$00,$00,$00
