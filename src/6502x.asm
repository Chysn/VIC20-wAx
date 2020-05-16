;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                                      wAx
;                           Extended Instruction Table
;                             (c)2020, Jason Justian
;                  
; Release 1 - May 15, 2020
; Assembled with XA
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
* = $a5be
; Tuplet and Char3 are used to decode instruction names            
Tuplet:     .asc "JMBEORLASANOBINSTADEBCPLDCLXSRTXJSEISLSHLBNBPHBROTSBMARRBVCMTY"
Padding1:   .asc "--"
Char3:      .asc "ACDEIKLPQRSTVXYO"

; 6502 Instructions
; Each instruction is encoded as three bytes.
; (1) The first byte is the 6502 opcode of the instruction
; (2) The second byte is the position of the first two characters of the 
;     instruction in the Tuple table
; (3) The third byte's low nybble is the position of the third character of
;     the instruction in the Char3 table. The high nybble is the addressing
;     mode of the insruction, as shown in the Constants labels at the top
;     of the source code
;
Instr6502:  .byte $69,$11,$a1   ; ADC #oper
            .byte $65,$11,$71   ; ADC oper
            .byte $75,$11,$81   ; ADC oper,X
            .byte $6d,$11,$41   ; ADC oper
            .byte $7d,$11,$51   ; ADC oper,X
            .byte $79,$11,$61   ; ADC oper,Y
            .byte $61,$11,$21   ; ADC (oper,X)
            .byte $71,$11,$31   ; ADC (oper),Y
            .byte $29,$09,$a2   ; AND #oper
            .byte $25,$09,$72   ; AND oper
            .byte $35,$09,$82   ; AND oper,X
            .byte $2d,$09,$42   ; AND oper
            .byte $3d,$09,$52   ; AND oper,X
            .byte $39,$09,$62   ; AND oper,Y
            .byte $21,$09,$22   ; AND (oper,X)
            .byte $31,$09,$32   ; AND (oper),Y
            .byte $0a,$07,$a6   ; ASL A
            .byte $06,$07,$76   ; ASL oper
            .byte $16,$07,$86   ; ASL oper,X
            .byte $0e,$07,$46   ; ASL oper
            .byte $1e,$07,$56   ; ASL oper,X
            .byte $90,$14,$c1   ; BCC oper
            .byte $b0,$14,$ca   ; BCS oper
            .byte $f0,$02,$c8   ; BEQ oper
            .byte $24,$0c,$7b   ; BIT oper
            .byte $2c,$0c,$4b   ; BIT oper
            .byte $30,$33,$c4   ; BMI oper
            .byte $d0,$29,$c3   ; BNE oper
            .byte $10,$2b,$c6   ; BPL oper
            .byte $00,$2e,$b5   ; BRK
            .byte $50,$38,$c1   ; BVC oper
            .byte $70,$38,$ca   ; BVS oper
            .byte $18,$19,$b1   ; CLC
            .byte $d8,$19,$b2   ; CLD
            .byte $58,$19,$b4   ; CLI
            .byte $b8,$19,$bc   ; CLV
            .byte $c9,$3a,$a7   ; CMP #oper
            .byte $c5,$3a,$77   ; CMP oper
            .byte $d5,$3a,$87   ; CMP oper,X
            .byte $cd,$3a,$47   ; CMP oper
            .byte $dd,$3a,$57   ; CMP oper,X
            .byte $d9,$3a,$67   ; CMP oper,Y
            .byte $c1,$3a,$27   ; CMP (oper,X)
            .byte $d1,$3a,$37   ; CMP (oper),Y
            .byte $e0,$15,$ad   ; CPX #oper
            .byte $e4,$15,$7d   ; CPX oper
            .byte $ec,$15,$4d   ; CPX oper
            .byte $c0,$15,$ae   ; CPY #oper
            .byte $c4,$15,$7e   ; CPY oper
            .byte $cc,$15,$4e   ; CPY oper
            .byte $c6,$12,$71   ; DEC oper
            .byte $d6,$12,$81   ; DEC oper,X
            .byte $ce,$12,$41   ; DEC oper
            .byte $de,$12,$51   ; DEC oper,X
            .byte $ca,$12,$bd   ; DEX
            .byte $88,$12,$be   ; DEY
            .byte $49,$03,$a9   ; EOR #oper
            .byte $45,$03,$79   ; EOR oper
            .byte $55,$03,$89   ; EOR oper,X
            .byte $4d,$03,$49   ; EOR oper
            .byte $5d,$03,$59   ; EOR oper,X
            .byte $59,$03,$69   ; EOR oper,Y
            .byte $41,$03,$29   ; EOR (oper,X)
            .byte $51,$03,$39   ; EOR (oper),Y
            .byte $e6,$0d,$71   ; INC oper
            .byte $f6,$0d,$81   ; INC oper,X
            .byte $ee,$0d,$41   ; INC oper
            .byte $fe,$0d,$51   ; INC oper,X
            .byte $e8,$0d,$bd   ; INX
            .byte $c8,$0d,$be   ; INY
            .byte $4c,$00,$47   ; JMP oper
            .byte $6c,$00,$17   ; JMP (oper)
            .byte $20,$20,$49   ; JSR oper
            .byte $a9,$17,$a0   ; LDA #oper
            .byte $a5,$17,$70   ; LDA oper
            .byte $b5,$17,$80   ; LDA oper,X
            .byte $ad,$17,$40   ; LDA oper
            .byte $bd,$17,$50   ; LDA oper,X
            .byte $b9,$17,$60   ; LDA oper,Y
            .byte $a1,$17,$20   ; LDA (oper,X)
            .byte $b1,$17,$30   ; LDA (oper),Y
            .byte $a2,$17,$ad   ; LDX #oper
            .byte $a6,$17,$7d   ; LDX oper
            .byte $b6,$17,$9d   ; LDX oper,Y
            .byte $ae,$17,$4d   ; LDX oper
            .byte $be,$17,$6d   ; LDX oper,Y
            .byte $a0,$17,$ae   ; LDY #oper
            .byte $a4,$17,$7e   ; LDY oper
            .byte $b4,$17,$8e   ; LDY oper,X
            .byte $ac,$17,$4e   ; LDY oper
            .byte $bc,$17,$5e   ; LDY oper,X
            .byte $4a,$25,$a9   ; LSR A
            .byte $46,$25,$79   ; LSR oper
            .byte $56,$25,$89   ; LSR oper,X
            .byte $4e,$25,$49   ; LSR oper
            .byte $5e,$25,$59   ; LSR oper,X
            .byte $ea,$0a,$b7   ; NOP
            .byte $09,$04,$a0   ; ORA #oper
            .byte $05,$04,$70   ; ORA oper
            .byte $15,$04,$80   ; ORA oper,X
            .byte $0d,$04,$40   ; ORA oper
            .byte $1d,$04,$50   ; ORA oper,X
            .byte $19,$04,$60   ; ORA oper,Y
            .byte $01,$04,$20   ; ORA (oper,X)
            .byte $11,$04,$30   ; ORA (oper),Y
            .byte $48,$2c,$b0   ; PHA
            .byte $08,$2c,$b7   ; PHP
            .byte $68,$16,$b0   ; PLA
            .byte $28,$16,$b7   ; PLP
            .byte $2a,$2f,$a6   ; ROL A
            .byte $26,$2f,$76   ; ROL oper
            .byte $36,$2f,$86   ; ROL oper,X
            .byte $2e,$2f,$46   ; ROL oper
            .byte $3e,$2f,$56   ; ROL oper,X
            .byte $6a,$2f,$a9   ; ROR A
            .byte $66,$2f,$79   ; ROR oper
            .byte $76,$2f,$89   ; ROR oper,X
            .byte $6e,$2f,$49   ; ROR oper
            .byte $7e,$2f,$59   ; ROR oper,X
            .byte $40,$1d,$b4   ; RTI
            .byte $60,$1d,$ba   ; RTS
            .byte $e9,$32,$a1   ; SBC #oper
            .byte $e5,$32,$71   ; SBC oper
            .byte $f5,$32,$81   ; SBC oper,X
            .byte $ed,$32,$41   ; SBC oper
            .byte $fd,$32,$51   ; SBC oper,X
            .byte $f9,$32,$61   ; SBC oper,Y
            .byte $e1,$32,$21   ; SBC (oper,X)
            .byte $f1,$32,$31   ; SBC (oper),Y
            .byte $38,$21,$b1   ; SEC
            .byte $f8,$21,$b2   ; SED
            .byte $78,$21,$b4   ; SEI
            .byte $85,$0f,$70   ; STA oper
            .byte $95,$0f,$80   ; STA oper,X
            .byte $8d,$0f,$40   ; STA oper
            .byte $9d,$0f,$50   ; STA oper,X
            .byte $99,$0f,$60   ; STA oper,Y
            .byte $81,$0f,$20   ; STA (oper,X)
            .byte $91,$0f,$30   ; STA (oper),Y
            .byte $86,$0f,$7d   ; STX oper
            .byte $96,$0f,$9d   ; STX oper,Y
            .byte $8e,$0f,$4d   ; STX oper
            .byte $84,$0f,$7e   ; STY oper
            .byte $94,$0f,$8e   ; STY oper,X
            .byte $8c,$0f,$4e   ; STY oper
            .byte $aa,$10,$bd   ; TAX
            .byte $a8,$10,$be   ; TAY
            .byte $ba,$31,$bd   ; TSX
            .byte $8a,$1e,$b0   ; TXA
            .byte $9a,$1e,$ba   ; TXS
            .byte $98,$3c,$b0   ; TYA
; Undocumented Opcodes
            .byte $0b,$09,$a1   ; ANC #arg
            .byte $2b,$09,$a1   ; ANC #arg
            .byte $87,$08,$7d   ; SAX arg
            .byte $97,$08,$9d   ; SAX arg,Y
            .byte $83,$08,$2d   ; SAX (arg,X)
            .byte $8f,$08,$4d   ; SAX arg
            .byte $6b,$35,$a9   ; ARR #arg
            .byte $4b,$07,$a9   ; ASR #arg
            .byte $ab,$1a,$a0   ; LXA #arg
            .byte $9f,$26,$60   ; SHA arg,Y
            .byte $93,$26,$30   ; SHA arg
            .byte $cb,$32,$ad   ; SBX #arg
            .byte $c7,$18,$77   ; DCP arg
            .byte $d7,$18,$87   ; DCP arg,X
            .byte $cf,$18,$47   ; DCP arg
            .byte $df,$18,$57   ; DCP arg,X
            .byte $db,$18,$67   ; DCP arg,Y
            .byte $c3,$18,$27   ; DCP (arg,X)
            .byte $d3,$18,$37   ; DCP (arg),Y
            .byte $04,$0a,$77   ; NOP arg
            .byte $14,$0a,$87   ; NOP arg,X
            .byte $34,$0a,$87   ; NOP arg,X
            .byte $44,$0a,$77   ; NOP arg
            .byte $54,$0a,$87   ; NOP arg,X
            .byte $64,$0a,$77   ; NOP arg
            .byte $74,$0a,$87   ; NOP arg,X
            .byte $80,$0a,$a7   ; NOP #arg
            .byte $82,$0a,$a7   ; NOP #arg
            .byte $89,$0a,$a7   ; NOP #arg
            .byte $c2,$0a,$a7   ; NOP #arg
            .byte $d4,$0a,$87   ; NOP arg,X
            .byte $e2,$0a,$a7   ; NOP #arg
            .byte $f4,$0a,$87   ; NOP arg,X
            .byte $e7,$23,$72   ; ISC arg
            .byte $f7,$23,$82   ; ISC arg,X
            .byte $ef,$23,$42   ; ISC arg
            .byte $ff,$23,$52   ; ISC arg,X
            .byte $fb,$23,$62   ; ISC arg,Y
            .byte $e3,$23,$22   ; ISC (arg,X)
            .byte $f3,$23,$32   ; ISC (arg),Y
            .byte $bb,$06,$63   ; LAE arg,Y
            .byte $a7,$06,$7d   ; LAX arg
            .byte $b7,$06,$9d   ; LAX arg,Y
            .byte $af,$06,$4d   ; LAX arg
            .byte $bf,$06,$6d   ; LAX arg,Y
            .byte $a3,$06,$2d   ; LAX (arg,X)
            .byte $b3,$06,$3d   ; LAX (arg),Y
            .byte $1a,$0a,$b7   ; NOP
            .byte $3a,$0a,$b7   ; NOP
            .byte $5a,$0a,$b7   ; NOP
            .byte $7a,$0a,$b7   ; NOP
            .byte $da,$0a,$b7   ; NOP
            .byte $fa,$0a,$b7   ; NOP
            .byte $27,$05,$70   ; RLA arg
            .byte $37,$05,$80   ; RLA arg,X
            .byte $2f,$05,$40   ; RLA arg
            .byte $3f,$05,$50   ; RLA arg,X
            .byte $3b,$05,$60   ; RLA arg,Y
            .byte $23,$05,$20   ; RLA (arg,X)
            .byte $33,$05,$30   ; RLA (arg),Y
            .byte $67,$36,$70   ; RRA arg
            .byte $77,$36,$80   ; RRA arg,X
            .byte $6f,$36,$40   ; RRA arg
            .byte $7f,$36,$50   ; RRA arg,X
            .byte $7b,$36,$60   ; RRA arg,Y
            .byte $63,$36,$20   ; RRA (arg,X)
            .byte $73,$36,$30   ; RRA (arg),Y
            .byte $eb,$32,$a1   ; SBC #byte
            .byte $07,$24,$7f   ; SLO arg
            .byte $17,$24,$8f   ; SLO arg,X
            .byte $0f,$24,$4f   ; SLO arg
            .byte $1f,$24,$5f   ; SLO arg,X
            .byte $1b,$24,$6f   ; SLO arg,Y
            .byte $03,$24,$2f   ; SLO (arg,X)
            .byte $13,$24,$3f   ; SLO (arg),Y
            .byte $47,$1c,$73   ; SRE arg
            .byte $57,$1c,$83   ; SRE arg,X
            .byte $4f,$1c,$43   ; SRE arg
            .byte $5f,$1c,$53   ; SRE arg,X
            .byte $5b,$1c,$63   ; SRE arg,Y
            .byte $43,$1c,$23   ; SRE (arg,X)
            .byte $53,$1c,$33   ; SRE (arg),Y
            .byte $9e,$26,$6d   ; SHX arg,Y
            .byte $9c,$26,$5e   ; SHY arg,X
            .byte $0c,$0a,$47   ; NOP arg
            .byte $1c,$0a,$57   ; NOP arg,X
            .byte $3c,$0a,$57   ; NOP arg,X
            .byte $5c,$0a,$57   ; NOP arg,X
            .byte $7c,$0a,$57   ; NOP arg,X
            .byte $dc,$0a,$57   ; NOP arg,X
            .byte $fc,$0a,$57   ; NOP arg,X
            .byte $8b,$09,$a3   ; ANE #arg
            .byte $9b,$26,$6a   ; SHS arg,Y
            .byte $02,$27,$bb   ; HLT
            .byte $f2           ; End of 6502 table
