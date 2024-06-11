SPRITE_P1_UP:
    ; UP 1        
    ; mask 0
    DB $07,$0F,$05,$1F,$1F,$3F,$3F,$7F
    DB $00,$4F,$70,$38,$0C,$0E,$07,$04
    DB $E0,$F0,$A0,$F8,$F8,$F8,$FC,$FE
    DB $06,$F2,$1E,$3C,$30,$70,$C0,$00
    ; mask 1
    DB $00,$00,$18,$00,$1F,$00,$00,$00
    DB $7F,$50,$4F,$3F,$0F,$0F,$07,$02
    DB $00,$00,$10,$00,$FC,$00,$00,$00
    DB $F2,$0E,$FE,$FC,$F0,$F0,$A0,$00
    ; mask 2
    DB $00,$00,$1A,$00,$00,$00,$00,$00
    DB $7F,$60,$00,$00,$00,$00,$07,$00
    DB $00,$00,$50,$00,$00,$00,$00,$00
    DB $FA,$0E,$00,$00,$00,$00,$80,$00
    
    ; UP 2
    ; mask 0
    DB $07,$0F,$05,$1F,$1F,$3F,$3F,$7F
    DB $10,$4F,$70,$38,$0C,$0E,$05,$00
    DB $E0,$F0,$A0,$F8,$F8,$F8,$FC,$FE
    DB $02,$F2,$1E,$3C,$30,$70,$E0,$40
    ; mask 1
    DB $00,$00,$18,$00,$1F,$00,$00,$00
    DB $4F,$70,$7F,$3F,$0F,$0F,$03,$00
    DB $00,$00,$10,$00,$FC,$00,$00,$00
    DB $FE,$06,$F2,$FC,$F0,$F0,$E0,$20
    ; mask 2
    DB $00,$00,$1A,$00,$00,$00,$00,$00
    DB $6F,$70,$00,$00,$00,$00,$01,$00
    DB $00,$00,$50,$00,$00,$00,$00,$00
    DB $FE,$0A,$00,$00,$00,$00,$E0,$00
    
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
    ; --- Slot 3
    ; mask 0
    DB $07,$0F,$02,$05,$07,$07,$0F,$0F
    DB $07,$03,$03,$03,$01,$03,$03,$04
    DB $E0,$F0,$80,$00,$A0,$E0,$C0,$C0
    DB $E0,$B0,$70,$F0,$F0,$F0,$C0,$00    
    ; mask 1
    DB $00,$00,$18,$1A,$08,$0A,$12,$10
    DB $08,$03,$02,$02,$01,$03,$03,$03
    DB $00,$00,$78,$F8,$5C,$1C,$7E,$7E
    DB $FE,$3E,$7E,$7C,$F8,$F8,$B0,$00
    ; mask 2
    DB $00,$00,$1D,$00,$28,$00,$00,$00
    DB $00,$07,$06,$06,$07,$07,$03,$00
    DB $00,$00,$78,$00,$5C,$00,$00,$00
    DB $00,$7E,$FE,$7C,$F8,$F8,$80,$00
    ; --- Slot 4
    ; mask 0
    DB $07,$0F,$02,$05,$07,$07,$0F,$0F
    DB $07,$03,$03,$03,$01,$03,$04,$00
    DB $E0,$F0,$80,$00,$A0,$E0,$C0,$C0
    DB $E0,$B0,$D0,$D0,$F0,$F0,$E0,$40
    ; mask 1
    DB $00,$00,$18,$1A,$08,$0A,$12,$10
    DB $08,$03,$03,$03,$01,$03,$03,$00
    DB $00,$00,$78,$F8,$5C,$1C,$7E,$7E
    DB $FE,$3E,$9E,$9C,$98,$F8,$E0,$30
    ; mask 2
    DB $00,$00,$1D,$00,$28,$00,$00,$00
    DB $00,$07,$07,$07,$07,$07,$00,$00
    DB $00,$00,$78,$00,$5C,$00,$00,$00
    DB $00,$7E,$BE,$BC,$98,$F8,$E0,$00

SPRITE_P1_RIGHT:
    ; --- Slot 0
    ; mask 0
    DB $07,$0F,$01,$00,$05,$07,$03,$03
    DB $07,$0D,$0E,$0F,$0F,$0F,$03,$00
    DB $E0,$F0,$40,$A0,$E0,$E0,$F0,$F0
    DB $E0,$C0,$C0,$C0,$80,$C0,$C0,$20
    ; mask 1
    DB $00,$00,$1E,$1F,$3A,$38,$7E,$7E
    DB $7F,$7C,$7E,$3E,$1F,$1F,$0D,$00
    DB $00,$00,$18,$58,$10,$50,$48,$08
    DB $10,$C0,$40,$40,$80,$C0,$C0,$C0
    ; mask 2
    DB $00,$00,$1E,$00,$3A,$00,$00,$00
    DB $00,$7E,$7F,$3E,$1F,$1F,$01,$00
    DB $00,$00,$B8,$00,$14,$00,$00,$00
    DB $00,$E0,$60,$60,$E0,$E0,$C0,$00
    
    ; --- Slot 2
   ; mask 0
    DB $07,$0F,$01,$00,$05,$07,$03,$03
    DB $07,$0D,$0B,$0B,$0F,$0F,$07,$02
    DB $E0,$F0,$40,$A0,$E0,$E0,$F0,$F0
    DB $E0,$C0,$C0,$C0,$80,$C0,$20,$00
    ; mask 1
    DB $00,$00,$1E,$1F,$3A,$38,$7E,$7E
    DB $7F,$7C,$79,$39,$19,$1F,$07,$0C
    DB $00,$00,$18,$58,$10,$50,$48,$08
    DB $10,$C0,$C0,$C0,$80,$C0,$C0,$00
        ; mask 2
    DB $00,$00,$1E,$00,$3A,$00,$00,$00
    DB $00,$7E,$7D,$3D,$19,$1F,$07,$00
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
    
    ; Antichrist - RIGHT
    ; mask 0 - 18h    
    DB $00,$00,$00,$01,$01,$01,$01,$01
    DB $01,$01,$07,$07,$01,$01,$00,$00
    DB $00,$00,$00,$80,$80,$80,$80,$80
    DB $80,$80,$E0,$E0,$80,$80,$00,$00

    ;Ghost 1 Left - 1CH
    ; mask 0
    DB $03,$04,$08,$68,$B5,$90,$50,$40
    DB $22,$20,$10,$10,$08,$06,$03,$00
    DB $C0,$E0,$70,$36,$BB,$33,$26,$06
    DB $0C,$18,$3E,$19,$07,$0C,$F8,$E0
    ; mask 1    20h
    DB $00,$07,$0F,$6F,$DF,$FF,$6F,$7F
    DB $3F,$3F,$1F,$1F,$0F,$05,$03,$00
    DB $00,$20,$90,$D6,$DD,$DD,$DA,$FA
    DB $F4,$E8,$CE,$EF,$FB,$F4,$18,$00
    
    ; Ghost 2 Left - 24h
    ; mask 0    24-h
    DB $03,$04,$08,$08,$65,$B0,$90,$40
    DB $22,$20,$10,$10,$08,$06,$01,$00
    DB $C0,$E0,$70,$30,$B6,$3B,$27,$06
    DB $0C,$18,$3C,$1A,$06,$0C,$F0,$C0
    ; mask 1    28h
    DB $00,$07,$0F,$0F,$6F,$DF,$EF,$7F
    DB $3F,$3F,$1F,$1F,$0F,$05,$01,$00
    DB $00,$20,$90,$D0,$D6,$DD,$D9,$FA
    DB $F4,$E8,$CC,$EE,$FA,$F4,$30,$00

    ; Ghost 1 Right - 2Ch
    ; mask 0
    DB $03,$07,$0E,$6C,$DD,$CC,$64,$60
    DB $30,$18,$7C,$98,$E0,$30,$1F,$07
    DB $C0,$20,$10,$16,$AD,$09,$0A,$02
    DB $44,$04,$08,$08,$10,$60,$C0,$00
    ; mask 1    - 30h
    DB $00,$04,$09,$6B,$BB,$BB,$5B,$5F
    DB $2F,$17,$73,$F7,$DF,$2F,$18,$00
    DB $00,$E0,$F0,$F6,$FB,$FF,$F6,$FE
    DB $FC,$FC,$F8,$F8,$F0,$A0,$C0,$00
    
    ; Ghost 2 Right - 34h
    ; mask 0
    DB $03,$07,$0E,$0C,$6D,$DC,$E4,$60
    DB $30,$18,$3C,$58,$60,$30,$0F,$03
    DB $C0,$20,$10,$10,$A6,$0D,$09,$02
    DB $44,$04,$08,$08,$10,$60,$80,$00
    ; mask 1    38h
    DB $00,$04,$09,$0B,$6B,$BB,$9B,$5F
    DB $2F,$17,$33,$77,$5F,$2F,$0C,$00
    DB $00,$E0,$F0,$F0,$F6,$FB,$F7,$FE
    DB $FC,$FC,$F8,$F8,$F0,$A0,$80,$00
    

    ; Esqueleto Left - 1
    ; mask 0 - 3Ch
    DB $1F,$1B,$2E,$24,$3F,$1F,$1F,$00
    DB $03,$05,$09,$04,$01,$02,$02,$04
    DB $80,$C0,$C0,$C0,$80,$00,$00,$80
    DB $E0,$D0,$C8,$C4,$C0,$20,$20,$40
    ; mask 1 - 40h
    DB $1F,$1F,$3F,$3F,$3F,$1E,$0A,$00
    DB $03,$05,$09,$04,$01,$00,$00,$00
    DB $00,$80,$80,$80,$00,$00,$00,$00
    DB $80,$10,$88,$84,$80,$00,$00,$00

    ; Esqueleto Left - 2
    ; mask 0 - 44h
    ; mask 0
    DB $0F,$0D,$17,$12,$1F,$0F,$0F,$00
    DB $03,$05,$09,$10,$01,$06,$08,$00
    DB $C0,$E0,$60,$60,$C0,$80,$80,$80
    DB $E0,$D0,$C8,$D0,$C0,$20,$20,$50
    ; mask 1 - 48h
    DB $0F,$0F,$1F,$1F,$1F,$0F,$05,$00
    DB $03,$05,$09,$10,$01,$00,$00,$00
    DB $80,$C0,$C0,$C0,$80,$00,$00,$00
    DB $80,$10,$88,$90,$80,$00,$20,$40
    
    ; Esqueleto Right - 1
    ; mask 0 - 4Ch
    DB $01,$03,$03,$03,$01,$00,$00,$01
    DB $07,$0B,$13,$23,$03,$04,$04,$02
    DB $F8,$D8,$74,$24,$FC,$F8,$F8,$00
    DB $C0,$A0,$90,$20,$80,$40,$40,$20
    ; mask 1 - 50h
    DB $00,$01,$01,$01,$00,$00,$00,$00
    DB $01,$08,$11,$21,$01,$00,$00,$00
    DB $F8,$F8,$FC,$FC,$FC,$78,$50,$00
    DB $C0,$A0,$90,$20,$80,$00,$00,$00
    
    ; Esqueleto Right - 2
    ; mask 0 - 54h
    DB $03,$07,$06,$06,$03,$01,$01,$01
    DB $07,$0B,$13,$0B,$03,$04,$04,$0A
    DB $F0,$B0,$E8,$48,$F8,$F0,$F0,$00
    DB $C0,$A0,$90,$08,$80,$60,$10,$00
    ; mask 1 - 58h
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
    ; Disparo - antichrist    
    DB $00,$00,$00,$0A,$0A,$0A,$0A,$0A
    DB $0A,$0A,$0A,$0A,$0A,$0A,$00,$00
    ; Fantasma
    ; attr 0
    DB $0F,$08,$08,$08,$08,$08,$08,$08
    DB $08,$08,$08,$08,$08,$08,$08,$0F
    ; attr 1
    DB $00,$47,$47,$47,$47,$47,$47,$47
    DB $47,$47,$47,$47,$47,$47,$47,$00
    
    
    ; Esqueleto Left 1
    ; attr 0
    DB $0E,$0E,$0E,$0E,$0E,$0E,$0E,$0E
    DB $0E,$0E,$0E,$0E,$0E,$0F,$0F,$0F
    ; attr 1
    DB $41,$49,$49,$49,$41,$41,$41,$00
    DB $41,$41,$41,$41,$41,$00,$00,$00

SPRITE_COLOR_P1_UP:
    ;UP 1
    ; attr 0
    DB $05,$05,$0A,$05,$05,$05,$05,$05
    DB $0A,$05,$0B,$0A,$0A,$0A,$08,$08
    ; attr 1
    DB $00,$00,$44,$00,$40,$00,$00,$00
    DB $44,$4A,$45,$45,$45,$45,$45,$45    
    ; attr 2
    DB $00,$00,$41,$00,$00,$00,$00,$00
    DB $41,$41,$00,$00,$00,$00,$42,$00
    
    ;SPRITE_COLOR_P1_UP2:
    ;UP 1
    ; attr 0
    DB $05,$05,$0A,$05,$05,$05,$05,$05
    DB $0A,$05,$0B,$0A,$0A,$0A,$08,$08
    ; attr 1
    DB $00,$00,$44,$00,$40,$00,$00,$00
    DB $44,$4A,$45,$45,$45,$45,$45,$45
    ; attr 2
    DB $00,$00,$41,$00,$00,$00,$00,$00
    DB $41,$41,$00,$00,$00,$00,$42,$00


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

stg1_gate:
    DB #00,#00,#00,#00,#00,#00
stg1_gate_blocked:
    DB #01,#01,#01,#01,#01,#01
mapa1:
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#00,#00,#00,#00,#00,#00,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        
      db #01,#00,#00,#00,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        
      db #01,#00,#00,#00,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#00,#00,#00,#00,#00,#00,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        
      db #01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#00,#00,#00,#00,#00,#00,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        
    
      db #01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#00,#00,#00,#00,#00,#00,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        
      db #01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#00,#00,#00,#00,#00,#00,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        
      db #01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#00,#00,#00,#00,#00,#00,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01        
      db #01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#00,#00,#01,#01        
      db #01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#00,#00,#01,#01        
      db #01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#00,#00,#01,#01        
      db #01,#01,#00,#00,#00,#00,#00,#01,#01,#01,#01,#01,#01,#00,#00,#00,#00,#00,#00,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01        
      db #01,#01,#00,#00,#00,#00,#00,#01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        

      db #01,#01,#00,#00,#00,#00,#01,#01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        
      db #01,#01,#00,#00,#00,#00,#01,#01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        
      db #01,#01,#00,#00,#01,#01,#01,#01,#01,#01,#01,#01,#01,#00,#00,#00,#00,#00,#00,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        
      db #01,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#00,#00,#00,#00,#00,#00,#01,#00,#00,#00,#00,#00,#00,#00,#00,#00,#00,#01,#01        
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#00,#00,#00,#00,#00,#00,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#00,#00,#00,#00,#00,#00,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01        

mapa2:
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#00,#00,#00,#00,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#00,#00,#00,#00,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#00,#00,#00,#00,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#00,#00,#00,#00,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#00,#00,#00,#00,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#00,#00,#00,#00,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#00,#00,#00,#00,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#00,#00,#00,#00,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#00,#00,#00,#00,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#00,#00,#00,#00,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#00,#00,#00,#00,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#00,#00,#00,#00,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#00,#00,#00,#00,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   
      db #01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#00,#00,#00,#00,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01,#01   

SPRITE_COLOR_P1_RED:
    DB $0A,$0F,$0A,$0F,$0A,$0F,$0A,$0F
    DB $0A,$0F,$0A,$0F,$0A,$0F,$0A,$0F

    DB $0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A
    DB $0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A

    DB $0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A
    DB $0A,$0A,$0A,$0A,$0A,$0A,$0A,$0A

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
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

    
PaletteData:
			;  data 1 (red 0-7; blue 0-7); data 2 (0000; green 0-7)
    db 0x02, 0x00 ; Color index 0
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
