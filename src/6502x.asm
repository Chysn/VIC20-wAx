;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                                      wAx
;                           Extended Instruction Table
;                             (c)2020, Jason Justian
;                  
; Release 1 - May 15, 2020
; Release 2 - May 24, 2020
; Assembled with XA
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
* = $a7fe
TABLE_END   = $f2

Extended:   .byte $0b,$87       ; ANC
            .byte $0b,$a0       ; * ANC #immediate
            .byte $2b,$a0       ; * ANC #immediate
            .byte $98,$71       ; SAX
            .byte $87,$70       ; * SAX zero page
            .byte $97,$90       ; * SAX zero page,y
            .byte $83,$20       ; * SAX (indirect,x)
            .byte $8f,$40       ; * SAX absolute
            .byte $0c,$a5       ; ARR
            .byte $6b,$a0       ; * ARR #immediate
            .byte $0c,$e5       ; ASR
            .byte $4b,$a0       ; * ASR #immediate
            .byte $66,$03       ; LXA
            .byte $ab,$a0       ; * LXA #immediate
            .byte $9a,$03       ; SHA
            .byte $9f,$60       ; * SHA absolute,y
            .byte $93,$30       ; * SHA (indirect),y
            .byte $98,$b1       ; SBX
            .byte $cb,$a0       ; * SBX #immediate
            .byte $20,$e1       ; DCP
            .byte $c7,$70       ; * DCP zero page
            .byte $d7,$80       ; * DCP zero page,x
            .byte $cf,$40       ; * DCP absolute
            .byte $df,$50       ; * DCP absolute,x
            .byte $db,$60       ; * DCP absolute,y
            .byte $c3,$20       ; * DCP (indirect,x)
            .byte $d3,$30       ; * DCP (indirect),y
            .byte $4c,$c5       ; ISB
            .byte $e7,$70       ; * ISB zero page
            .byte $f7,$80       ; * ISB zero page,x
            .byte $ef,$40       ; * ISB absolute
            .byte $ff,$50       ; * ISB absolute,x
            .byte $fb,$60       ; * ISB absolute,y
            .byte $e3,$20       ; * ISB (indirect,x)
            .byte $f3,$30       ; * ISB (indirect),y
            .byte $60,$4b       ; LAE
            .byte $bb,$60       ; * LAE absolute,y
            .byte $60,$71       ; LAX
            .byte $a7,$70       ; * LAX zero page
            .byte $b7,$90       ; * LAX zero page,y
            .byte $af,$40       ; * LAX absolute
            .byte $bf,$60       ; * LAX absolute,y
            .byte $a3,$20       ; * LAX (indirect,x)
            .byte $b3,$30       ; * LAX (indirect),y
            .byte $73,$e1       ; NOP
            .byte $1a,$b0       ; * NOP 
            .byte $3a,$b0       ; * NOP 
            .byte $5a,$b0       ; * NOP 
            .byte $7a,$b0       ; * NOP 
            .byte $da,$b0       ; * NOP 
            .byte $fa,$b0       ; * NOP 
            .byte $04,$70       ; * NOP zero page
            .byte $14,$80       ; * NOP zero page,x
            .byte $34,$80       ; * NOP zero page,x
            .byte $44,$70       ; * NOP zero page
            .byte $54,$80       ; * NOP zero page,x
            .byte $64,$70       ; * NOP zero page
            .byte $74,$80       ; * NOP zero page,x
            .byte $80,$a0       ; * NOP #immediate
            .byte $82,$a0       ; * NOP #immediate
            .byte $89,$a0       ; * NOP #immediate
            .byte $c2,$a0       ; * NOP #immediate
            .byte $d4,$80       ; * NOP zero page,x
            .byte $e2,$a0       ; * NOP #immediate
            .byte $f4,$80       ; * NOP zero page,x
            .byte $0c,$40       ; * NOP absolute
            .byte $1c,$50       ; * NOP absolute,x
            .byte $3c,$50       ; * NOP absolute,x
            .byte $5c,$50       ; * NOP absolute,x
            .byte $7c,$50       ; * NOP absolute,x
            .byte $dc,$50       ; * NOP absolute,x
            .byte $fc,$50       ; * NOP absolute,x            
            .byte $93,$03       ; RLA
            .byte $27,$70       ; * RLA zero page
            .byte $37,$80       ; * RLA zero page,x
            .byte $2f,$40       ; * RLA absolute
            .byte $3f,$50       ; * RLA absolute,x
            .byte $3b,$60       ; * RLA absolute,y
            .byte $23,$20       ; * RLA (indirect,x)
            .byte $33,$30       ; * RLA (indirect),y
            .byte $94,$83       ; RRA
            .byte $67,$70       ; * RRA zero page
            .byte $77,$80       ; * RRA zero page,x
            .byte $6f,$40       ; * RRA absolute
            .byte $7f,$50       ; * RRA absolute,x
            .byte $7b,$60       ; * RRA absolute,y
            .byte $63,$20       ; * RRA (indirect,x)
            .byte $73,$30       ; * RRA (indirect),y
            .byte $98,$87       ; SBC
            .byte $eb,$a0       ; * SBC #immediate
            .byte $9b,$1f       ; SLO
            .byte $07,$70       ; * SLO zero page
            .byte $17,$80       ; * SLO zero page,x
            .byte $0f,$40       ; * SLO absolute
            .byte $1f,$50       ; * SLO absolute,x
            .byte $1b,$60       ; * SLO absolute,y
            .byte $03,$20       ; * SLO (indirect,x)
            .byte $13,$30       ; * SLO (indirect),y
            .byte $9c,$8b       ; SRE
            .byte $47,$70       ; * SRE zero page
            .byte $57,$80       ; * SRE zero page,x
            .byte $4f,$40       ; * SRE absolute
            .byte $5f,$50       ; * SRE absolute,x
            .byte $5b,$60       ; * SRE absolute,y
            .byte $43,$20       ; * SRE (indirect,x)
            .byte $53,$30       ; * SRE (indirect),y
            .byte $9a,$31       ; SHX
            .byte $9e,$60       ; * SHX absolute,y
            .byte $9a,$33       ; SHY
            .byte $9c,$50       ; * SHY absolute,x
            .byte $0b,$8b       ; ANE
            .byte $8b,$a0       ; * ANE #immediate
            .byte $9a,$27       ; SHS
            .byte $9b,$60       ; * SHS absolute,y
            .byte $50,$5b       ; JAM
            .byte $02,$b0       ; * JAM            
            .byte $43,$29       ; HLT
            .byte $02,$b0       ; * HLT 
            .byte TABLE_END     ; End of 6502 extended table
