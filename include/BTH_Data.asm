SPRITE_P1_UP:
    ; UP 1        
    ; mask 0    
    DB $07,$0F,$05,$1F,$1F,$3F,$3F,$7F
    DB $00,$60,$70,$38,$0C,$0E,$07,$04
    DB $E0,$F0,$A0,$F8,$F8,$F8,$FC,$FE
    DB $02,$06,$1E,$3C,$30,$70,$C0,$00
    ; mask 1
    DB $00,$00,$18,$00,$1F,$00,$00,$00
    DB $7F,$7F,$7F,$3F,$0F,$0F,$07,$02
    DB $00,$00,$10,$00,$FC,$00,$00,$00
    DB $FE,$FE,$FE,$FC,$F0,$F0,$A0,$00
    ; mask 2
    DB $00,$00,$1A,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$07,$00
    DB $00,$00,$50,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$80,$00
    
    ; UP 2
    ; mask 0
    DB $07,$0F,$05,$1F,$1F,$3F,$3F,$7F
    DB $00,$60,$70,$38,$0C,$0E,$05,$00
    DB $E0,$F0,$A0,$F8,$F8,$F8,$FC,$FE
    DB $02,$06,$1E,$3C,$30,$70,$E0,$40
    ; mask 1
    DB $00,$00,$18,$00,$1F,$00,$00,$00
    DB $7F,$7F,$7F,$3F,$0F,$0F,$03,$00
    DB $00,$00,$10,$00,$FC,$00,$00,$00
    DB $FE,$FE,$FE,$FC,$F0,$F0,$E0,$20
    ; mask 2
    DB $00,$00,$1A,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$01,$00
    DB $00,$00,$50,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$E0,$00
    
SPRITE_P1_DOWN:
    ; DOWN 1 - 00h
    ; mask 0
    DB $07,$0F,$04,$06,$0F,$07,$1F,$3F
    DB $4C,$4E,$4E,$3C,$0C,$0E,$0F,$08
    DB $E0,$F0,$20,$20,$70,$E0,$F8,$FC
    DB $32,$72,$7E,$7C,$70,$F0,$C0,$00    
    ; mask 1
    DB $00,$00,$19,$19,$10,$3A,$3A,$78
    DB $5F,$5E,$7E,$3C,$0C,$0E,$0F,$04
    DB $00,$00,$90,$D8,$88,$58,$5C,$1E
    DB $F6,$FE,$FE,$3C,$30,$70,$A0,$00    
    ; mask 2
    DB $00,$00,$1B,$00,$10,$00,$00,$00
    DB $6F,$6F,$7F,$3F,$0F,$0F,$0F,$00
    DB $00,$00,$D0,$00,$8C,$00,$00,$00
    DB $FA,$7E,$7E,$BC,$B0,$70,$80,$00
    
    ; DOWN 2
    ; mask 0 - 0Ch    
    DB $07,$0F,$04,$06,$0F,$07,$1F,$3F
    DB $4C,$4E,$7E,$3C,$0C,$0E,$05,$00
    DB $E0,$F0,$20,$20,$70,$E0,$F8,$FC
    DB $32,$72,$72,$7C,$70,$F0,$E0,$40    
    ; mask 1
    DB $00,$00,$19,$19,$10,$3A,$3A,$78
    DB $5F,$7E,$7E,$3C,$0C,$0E,$03,$00
    DB $00,$00,$90,$D8,$88,$58,$5C,$1E
    DB $F6,$F6,$FE,$3C,$30,$70,$E0,$20    
    ; mask 2
    DB $00,$00,$1B,$00,$10,$00,$00,$00
    DB $6F,$7F,$7F,$3F,$0F,$0F,$01,$00
    DB $00,$00,$D0,$00,$8C,$00,$00,$00
    DB $FA,$7A,$7E,$BC,$B0,$70,$E0,$00

SPRITE_P1_LEFT:         
    ; --- Slot 1
    ; mask 0
    DB $07,$0F,$02,$05,$07,$07,$0F,$0F
    DB $07,$03,$03,$01,$01,$03,$03,$04
    DB $E0,$F0,$80,$00,$A0,$E0,$C0,$C0
    DB $E0,$B0,$B0,$F0,$F0,$F0,$C0,$00
    ; mask 1
    DB $00,$00,$18,$1A,$08,$0A,$12,$10
    DB $08,$03,$03,$01,$01,$03,$03,$03
    DB $00,$00,$78,$F8,$5C,$1C,$7E,$7E
    DB $FE,$3E,$3E,$3C,$F8,$F8,$B0,$00    
    ; mask 2
    DB $00,$00,$1D,$00,$28,$00,$00,$00
    DB $00,$07,$07,$07,$07,$07,$03,$00
    DB $00,$00,$78,$00,$5C,$00,$00,$00
    DB $00,$7E,$7E,$3C,$F8,$F8,$80,$00    
    ; --- Slot 3
    ; mask 0
    DB $07,$0F,$02,$05,$07,$07,$0F,$0F
    DB $07,$03,$03,$01,$01,$03,$04,$00
    DB $E0,$F0,$80,$00,$A0,$E0,$C0,$C0
    DB $E0,$B0,$B0,$B0,$F0,$F0,$E0,$40
    ; mask 1
    DB $00,$00,$18,$1A,$08,$0A,$12,$10
    DB $08,$03,$03,$01,$01,$03,$03,$00
    DB $00,$00,$78,$F8,$5C,$1C,$7E,$7E
    DB $FE,$3E,$3E,$3C,$38,$F8,$E0,$30
    ; mask 2
    DB $00,$00,$1D,$00,$28,$00,$00,$00
    DB $00,$07,$07,$07,$07,$07,$00,$00
    DB $00,$00,$78,$00,$5C,$00,$00,$00
    DB $00,$7E,$7E,$7C,$38,$F8,$E0,$00

SPRITE_P1_RIGHT:
    ; --- Slot 0
    ; mask 0
    DB $07,$0F,$01,$00,$05,$07,$03,$03
    DB $07,$0D,$0D,$0F,$0F,$0F,$03,$00
    DB $E0,$F0,$40,$A0,$E0,$E0,$F0,$F0
    DB $E0,$C0,$C0,$80,$80,$C0,$C0,$20    
    ; mask 1
    DB $00,$00,$1E,$1F,$3A,$38,$7E,$7E
    DB $7F,$7C,$7C,$3C,$1F,$1F,$0D,$00
    DB $00,$00,$18,$58,$10,$50,$48,$08
    DB $10,$C0,$C0,$80,$80,$C0,$C0,$C0    
    ; mask 2
    DB $00,$00,$1E,$00,$3A,$00,$00,$00
    DB $00,$7E,$7E,$3C,$1F,$1F,$01,$00
    DB $00,$00,$B8,$00,$14,$00,$00,$00
    DB $00,$E0,$E0,$E0,$E0,$E0,$C0,$00
    
    ; --- Slot 2
    ; mask 0
    DB $07,$0F,$01,$00,$05,$07,$03,$03
    DB $07,$0D,$0D,$0D,$0F,$0F,$07,$02
    DB $E0,$F0,$40,$A0,$E0,$E0,$F0,$F0
    DB $E0,$C0,$C0,$80,$80,$C0,$20,$00
    ; mask 1
    DB $00,$00,$1E,$1F,$3A,$38,$7E,$7E
    DB $7F,$7C,$7C,$3C,$1C,$1F,$07,$0C
    DB $00,$00,$18,$58,$10,$50,$48,$08
    DB $10,$C0,$C0,$80,$80,$C0,$C0,$00
    ; mask 2
    DB $00,$00,$1E,$00,$3A,$00,$00,$00
    DB $00,$7E,$7E,$3E,$1C,$1F,$07,$00
    DB $00,$00,$B8,$00,$14,$00,$00,$00
    DB $00,$E0,$E0,$E0,$E0,$E0,$00,$00

SPRITE_PATTERN:    
    ; P1 - DOWN 1 
    ; DOWN 1 - 00h
    ; mask 0
    DB $07,$0F,$04,$06,$0F,$07,$1F,$3F
    DB $4C,$4E,$4E,$3C,$0C,$0E,$0F,$08
    DB $E0,$F0,$20,$20,$70,$E0,$F8,$FC
    DB $32,$72,$7E,$7C,$70,$F0,$C0,$00    
    ; mask 1
    DB $00,$00,$19,$19,$10,$3A,$3A,$78
    DB $5F,$5E,$7E,$3C,$0C,$0E,$0F,$04
    DB $00,$00,$90,$D8,$88,$58,$5C,$1E
    DB $F6,$FE,$FE,$3C,$30,$70,$A0,$00    
    ; mask 2
    DB $00,$00,$1B,$00,$10,$00,$00,$00
    DB $6F,$6F,$7F,$3F,$0F,$0F,$0F,$00
    DB $00,$00,$D0,$00,$8C,$00,$00,$00
    DB $FA,$7E,$7E,$BC,$B0,$70,$80,$00
    
    ; DOWN 2
    ; mask 0 - 0Ch    
    DB $07,$0F,$04,$06,$0F,$07,$1F,$3F
    DB $4C,$4E,$7E,$3C,$0C,$0E,$05,$00
    DB $E0,$F0,$20,$20,$70,$E0,$F8,$FC
    DB $32,$72,$72,$7C,$70,$F0,$E0,$40    
    ; mask 1
    DB $00,$00,$19,$19,$10,$3A,$3A,$78
    DB $5F,$7E,$7E,$3C,$0C,$0E,$03,$00
    DB $00,$00,$90,$D8,$88,$58,$5C,$1E
    DB $F6,$F6,$FE,$3C,$30,$70,$E0,$20    
    ; mask 2
    DB $00,$00,$1B,$00,$10,$00,$00,$00
    DB $6F,$7F,$7F,$3F,$0F,$0F,$01,$00
    DB $00,$00,$D0,$00,$8C,$00,$00,$00
    DB $FA,$7A,$7E,$BC,$B0,$70,$E0,$00
    
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

    ; Esqueleto Left - 1
    ; mask 0 - 2Ch
    DB $1F,$1B,$2E,$24,$3F,$1F,$1F,$00
    DB $03,$05,$09,$04,$01,$02,$02,$04
    DB $80,$C0,$C0,$C0,$80,$00,$00,$80
    DB $E0,$D0,$C8,$C4,$C0,$20,$20,$40
    ; mask 1 - 30h
    DB $1F,$1F,$3F,$3F,$3F,$1E,$0A,$00
    DB $03,$05,$09,$04,$01,$00,$00,$00
    DB $00,$80,$80,$80,$00,$00,$00,$00
    DB $80,$10,$88,$84,$80,$00,$00,$00

    ; Esqueleto Left - 2
    ; mask 0 - 34h
    ; mask 0
    DB $0F,$0D,$17,$12,$1F,$0F,$0F,$00
    DB $03,$05,$09,$10,$01,$06,$08,$00
    DB $C0,$E0,$60,$60,$C0,$80,$80,$80
    DB $E0,$D0,$C8,$D0,$C0,$20,$20,$50
    ; mask 1 - 38h
    DB $0F,$0F,$1F,$1F,$1F,$0F,$05,$00
    DB $03,$05,$09,$10,$01,$00,$00,$00
    DB $80,$C0,$C0,$C0,$80,$00,$00,$00
    DB $80,$10,$88,$90,$80,$00,$20,$40
    
    ; Esqueleto Right - 1
    ; mask 0 - 3Ch
    DB $01,$03,$03,$03,$01,$00,$00,$01
    DB $07,$0B,$13,$23,$03,$04,$04,$02
    DB $F8,$D8,$74,$24,$FC,$F8,$F8,$00
    DB $C0,$A0,$90,$20,$80,$40,$40,$20
    ; mask 1 - 40h
    DB $00,$01,$01,$01,$00,$00,$00,$00
    DB $01,$08,$11,$21,$01,$00,$00,$00
    DB $F8,$F8,$FC,$FC,$FC,$78,$50,$00
    DB $C0,$A0,$90,$20,$80,$00,$00,$00
    
    ; Esqueleto Right - 2
    ; mask 0 - 44h
    DB $03,$07,$06,$06,$03,$01,$01,$01
    DB $07,$0B,$13,$0B,$03,$04,$04,$0A
    DB $F0,$B0,$E8,$48,$F8,$F0,$F0,$00
    DB $C0,$A0,$90,$08,$80,$60,$10,$00
    ; mask 1 - 48h
    DB $01,$03,$03,$03,$01,$00,$00,$00
    DB $01,$08,$11,$09,$01,$00,$04,$02
    DB $F0,$F0,$F8,$F8,$F8,$F0,$A0,$00
    DB $C0,$A0,$90,$08,$80,$00,$00,$00


SPRITE_COLOR_TABLE:       
     ;P1 - DOWN 1
    ;DOWN 1
    ; attr 0
    DB $05,$05,$0A,$0B,$0B,$0B,$0B,$0B
    DB $04,$04,$04,$0A,$0A,$0A,$08,$08    
    ; attr 1
    DB $00,$00,$44,$45,$45,$45,$45,$45
    DB $4A,$4A,$4A,$44,$44,$44,$45,$45    
    ; attr 2
    DB $00,$00,$41,$00,$40,$00,$00,$00
    DB $41,$41,$41,$41,$41,$41,$42,$00
         
    ; Cada posición ha de corresponder con la tabla de atributos    

    ; Fantasma
    DB $01,$01,$01,$01,$01,$01,$01,$01
    DB $01,$01,$01,$01,$01,$01,$01,$01
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
    DB $00,$00,$00,$0A,$0A,$0A,$0A,$0A
    DB $0A,$0A,$0A,$0A,$0A,$0A,$00,$00
    
    ; Esqueleto Left 1
    ; attr 0
    DB $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    DB $0E,$0E,$0E,$0E,$0E,$0F,$0F,$0F
    ; attr 1
    DB $41,$49,$49,$49,$41,$41,$41,$00
    DB $41,$41,$41,$41,$41,$00,$00,$00
    ; Esqueleto Left 2
    ; attr 0
    DB $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    DB $0E,$0E,$0E,$0E,$0E,$0F,$0E,$0E
    ; attr 1
    DB $41,$49,$49,$49,$41,$41,$41,$00
    DB $41,$41,$41,$41,$41,$00,$41,$41

    ; Esqueleto Right 1
    ; attr 0
    ;DB $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    ;DB $0E,$0E,$0E,$0E,$0E,$0F,$0E,$0E    
    ; attr 1
    ;DB $41,$49,$49,$49,$41,$41,$41,$41
    ;DB $41,$41,$41,$41,$41,$00,$41,$41
    ; Esqueleto Right 2
    ; attr 0
    ;DB $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    ;DB $0E,$0E,$0E,$0E,$0E,$0F,$0F,$0F
    ; attr 1
    ;DB $41,$49,$49,$49,$41,$41,$41,$41
    ;DB $41,$41,$41,$41,$41,$00,$00,$00

SPRITE_COLOR_P1_UP:
    ;UP 1
    ; attr 0
    DB $05,$05,$0A,$05,$05,$05,$05,$05
    DB $0A,$0A,$0A,$0A,$0A,$0A,$08,$08    
    ; attr 1
    DB $00,$00,$44,$00,$40,$00,$00,$00
    DB $45,$45,$45,$45,$45,$45,$45,$45
    ; attr 2
    DB $00,$00,$41,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$42,$00
    
    ;SPRITE_COLOR_P1_UP2:
    ;UP 1
    ; attr 0
    DB $05,$05,$0A,$05,$05,$05,$05,$05
    DB $0A,$0A,$0A,$0A,$0A,$0A,$08,$08
    ; attr 1
    DB $00,$00,$44,$00,$40,$00,$00,$00
    DB $45,$45,$45,$45,$45,$45,$45,$45
    ; attr 2
    DB $00,$00,$41,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$42,$00

SPRITE_COLOR_P1_DOWN:
    ;DOWN 1
    ; attr 0
    DB $05,$05,$0A,$0B,$0B,$0B,$0B,$0B
    DB $04,$04,$04,$0A,$0A,$0A,$08,$08    
    ; attr 1
    DB $00,$00,$44,$45,$45,$45,$45,$45
    DB $4A,$4A,$4A,$44,$44,$44,$45,$45    
    ; attr 2
    DB $00,$00,$41,$00,$40,$00,$00,$00
    DB $41,$41,$41,$41,$41,$41,$42,$00
    ;SPRITE_COLOR_P1_DOWN2:
    ;DOWN 1
    ; attr 0
    DB $05,$05,$0A,$0B,$0B,$0B,$0B,$0B
    DB $04,$04,$04,$0A,$0A,$0A,$08,$08    
    ; attr 1
    DB $00,$00,$44,$45,$45,$45,$45,$45
    DB $4A,$4A,$4A,$44,$44,$44,$45,$45    
    ; attr 2
    DB $00,$00,$41,$00,$40,$00,$00,$00
    DB $41,$41,$41,$41,$41,$41,$42,$00

SPRITE_COLOR_P1_RIGHT:
    ; attr 0
    DB $05,$05,$0A,$0B,$0B,$0B,$0B,$0B
    DB $0B,$0A,$0A,$0B,$0A,$0A,$08,$08
    ; attr 1
    DB $00,$00,$44,$45,$45,$45,$45,$45
    DB $45,$44,$44,$44,$44,$44,$45,$45
    ; attr 2
    DB $00,$00,$41,$00,$40,$00,$00,$00
    DB $00,$41,$41,$41,$41,$41,$42,$00

    ; attr 0
    DB $05,$05,$0A,$0B,$0B,$0B,$0B,$0B
    DB $0B,$0A,$0A,$0A,$0B,$0A,$08,$08
    ; attr 1
    DB $00,$00,$44,$45,$45,$45,$45,$45
    DB $45,$44,$44,$44,$44,$44,$45,$45
    ; attr 2
    DB $00,$00,$41,$00,$40,$00,$00,$00
    DB $00,$41,$41,$41,$41,$41,$42,$00

mapa1:
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#00,#00,#00,#00,#00,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#00,#00,#00,#00,#00,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        
      db #01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        
      db #01,#01,#00,#00,#01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        
      db #01,#01,#00,#00,#01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        
      db #01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        
      db #01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        
    
      db #01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        
      db #01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        
      db #01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01,#00,#00,#00,#00,#00,#00,#01,#01,#00,#00,#00,#00,#01,#01        
      db #01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01,#00,#00,#00,#00,#00,#00,#01,#01,#00,#00,#00,#00,#01,#01        
      db #01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        
      db #01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        
      db #01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        
      db #01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        

      db #01,#01,#00,#00,#00,#00,#01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        
      db #01,#01,#00,#00,#00,#00,#01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        
      db #01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        
      db #01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   

      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   

mapa2:
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
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#00,#00,#00,#00,#00,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   


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
    DB $05,$05,$0A,$0B,$0B,$0B,$0B,$0B
    DB $0B,$0A,$0A,$0B,$0A,$0A,$08,$08
    ; attr 1
    DB $00,$00,$44,$45,$45,$45,$45,$45
    DB $45,$44,$44,$44,$44,$44,$45,$45
    ; attr 2
    DB $00,$00,$41,$00,$40,$00,$00,$00
    DB $00,$41,$41,$41,$41,$41,$42,$00
    ;SPRITE_COLOR_P1_LEFT2:    
    ; attr 0
    DB $05,$05,$0A,$0B,$0B,$0B,$0B,$0B
    DB $0B,$0A,$0A,$0A,$0B,$0A,$08,$08    
    ; attr 1
    DB $00,$00,$44,$45,$45,$45,$45,$45
    DB $45,$44,$44,$44,$44,$44,$45,$45    
    ; attr 2
    DB $00,$00,$41,$00,$40,$00,$00,$00
    DB $00,$41,$41,$41,$41,$41,$42,$00


BLANK_DATA:
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    
PaletteData:
			;  data 1 (red 0-7; blue 0-7); data 2 (0000; green 0-7)
    db 0x00, 0x00 ; Color index 0
    db 0x30, 0x00 ; Color index 1
    db 0x70, 0x06 ; Color index 2
    db 0x50, 0x00 ; Color index 3
    db 0x11, 0x01 ; Color index 4
    db 0x22, 0x02 ; Color index 5
    db 0x06, 0x02 ; Color index 6
    db 0x77, 0x07 ; Color index 7
    db 0x33, 0x03 ; Color index 8
    db 0x66, 0x06 ; Color index 9
    db 0x70, 0x00 ; Color index 10
    db 0x64, 0x05 ; Color index 11
    db 0x50, 0x03 ; Color index 12
    db 0x02, 0x04 ; Color index 13
    db 0x01, 0x02 ; Color index 14
    db 0x00, 0x00 ; Color index 15