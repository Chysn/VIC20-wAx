; wAx - Wedge Assembler
* = $a000

; Configuration
WCHAR   = $24                   ; Wedge character $
QUOTE   = $22                   ; Quote character

; System Resources
IGONE       = $0308             ; Vector to GONE
GONE        = $c7e4
CHRGET      = $0073
BUF         = $0200             ; Input buffer
PRTSTR      = $cb1e             ; Print from data (Y,A)
CHROUT      = $ffd2             ; Output one character
BUFPTR      = $7a               ; Pointer to buffer

; Constants
; Addressing mode encodings
ABSOLUTE    = $10
ABSOLUTE_X  = $30
ABSOLUTE_Y  = $50
ZEROPAGE    = $00
ZEROPAGE_X  = $20
ZEROPAGE_Y  = $40
IMMEDIATE   = $60
IMPLIED     = $70
INDIRECT    = $90
INDIRECT_X  = $a0
INDIRECT_Y  = $c0
RELATIVE    = $b0

; Assembler Workspace
WORK        = $aa               ; Temporary workspace (2 bytes)
TARGET      = $ac               ; Target assembly address (2 bytes)
MNEMONIC    = $ae               ; Mneumonic/addressing mode encoding (2 bytes)
OPERAND     = $b0               ; Operand storage
                            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; INSTALLER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
Install:    lda #<Assemble
            sta IGONE
            lda #>Assemble
            sta IGONE+1
            lda #<Intro
            ldy #>Intro
            jsr PRTSTR
            rts
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; MAIN PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;              
Assemble:   jsr CHRGET
            cmp #WCHAR
            beq Prepare
            jmp GONE+3          ; +3 because jsr CHRGET is done

Prepare:    ldx #$00
-loop:      lda WORK,x
            pha
            inx
            cpx #$08
            bne loop

; Get target address from the first four characters after the wedge
;
GetTarget:  jsr Buff2Byte       ; Convert 2 characters to a byte
            sta TARGET+1        ; Save to the target high byte
            jsr Buff2Byte       ; Convert next 2 characters to byte
            sta TARGET          ; Save to the target low byte

; Get mnemonic from the next three characters
GetMnemon:  jsr CHRGET          ; Quote is required because of keywords
            jsr GetAlpha        ; First character of the mnemonic is just
            sta MNEMONIC        ;   added to the low byte
            lda #$00            ; Clear the high bytes of the workspace, for
            sta WORK+1          ;   the second character's encoding
            jsr GetAlpha        ; The second character of the mnemonic is 
            sta WORK            ;   shifted 5 bites, to multiply it by 32 in 
            ldx #$05            ;   a 16-bit register
-loop:      asl WORK            ;   ,,
            rol WORK+1          ;   ,,
            dex                 ;   ,,
            bne loop            ;   ,,
            clc                 ; Add the workspace 16-bit register to the
            lda MNEMONIC        ;   mnemonic 16-bit register.
            adc WORK            ;   ,,
            sta MNEMONIC        ;   ,,
            lda #$00            ;   ,,
            adc WORK+1          ;   ,,
            sta MNEMONIC+1      ;   ,,
            jsr GetAlpha        ; The third character of the mnemonic is
            asl                 ;   multiplied by two and added to the low byte
            cmp #$30            ; If the third character is 'X', add one to
            beq plus1           ;   the value by not clearing the Carry flag.
            clc                 ;   This avoids encoding collisions.
-plus1:     adc MNEMONIC
            sta MNEMONIC
            bcc GetAddMode
            inc MNEMONIC+1

; Get addressing mode based on what's next
; * If #, force to IMMEDIATE
; * If (, it is one of the INDIRECT modes
; * If $, it is ABSOLUTE, RELATIVE, or ZEROPAGE
; * If nothing, then it's IMPLIED (which subsumes ACCCUMULATOR)          
GetAddMode: jsr CHRGET
ch_imm:     cmp #"#"            ; # indicates immediate mode
            bne ch_ind
            jmp AsmImm
ch_ind:     cmp #"("            ; ( indicates indirect mode
            bne ch_imp
            jmp AsmInd
ch_imp:     cmp #QUOTE          ; " indicates implied mode
            bne ch_acc
            jmp AsmImp
ch_acc:     cmp #"A"            ; A indicates accumulator mode
            bne ch_abs
            jmp AsmImp
ch_abs:     cmp #"$"            ; $ indicates absolute mode
            bne AsmFail
            jmp AsmAbs

; Opcode not found or formatting is invalid
AsmFail     lda #"?"
            jsr CHROUT
            
; Return the zero-page working space to its original state
;      
Return:     ldx #$07
-loop:      pla
            sta WORK,x
            dex
            bpl loop
readout:    jsr CHRGET
            bne readout            
            jmp GONE+3          ; Continue parsing with IGONE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; ADDRESSING MODE HANDLERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Assemble Immediate
AsmImm:     jsr CHRGET
            cmp #"$"
            bne AsmFail
            jsr GetOperand
            lda #IMMEDIATE
            jsr SetMode
            jsr OpLookup
            bcc AsmFail
            ldy #$00
            sta (TARGET),y
            lda OPERAND
            iny
            sta (TARGET),y
            jmp Return
     
; Assemble Implied
; And, because accumulator mode has a syntactical similarity, this also
; handles that mode            
AsmImp:     lda #IMPLIED
            jsr SetMode
            jsr OpLookup
            bcc AsmFail
            ldy #$00
            sta (TARGET),Y
            jmp Return    

; Assemble Relative
; Dispatched from from AsmAbs
AsmRel:     ldy #$00            ; Assemble the mnemonic
            sta (TARGET),y      ; ,,
            lda OPERAND         ; Get the instruction operand
            sec                 ; Subtract the target assembly address
            sbc TARGET          ;   from the instruction target to
            iny                 ;   get the relative branch's operand.
            sec                 
            sbc #$02            ; Offset by 2 to account for the instruction
            sta (TARGET),y
            jmp Return
            
; Assemble Asbolute
; And, because relative mode has a syntactical similarity, this also checks
; for availablity of that mode            
AsmAbs:     jsr GetOperand
            lda #RELATIVE       ; See if this mnemonic has a relative
            jsr SetMode         ;   opcode associated with it. If so, it's
            jsr OpLookup        ;   a relative branch.
            bcs AsmRel
            lda #ABSOLUTE       ; Set the starting addressing mode
modify:     jsr CheckForXY      ; Modify the addressing mode if X or Y are found
            ldy OPERAND+1       ; Optimize to zero-page: if high byte of
            bne variant         ;   operand is 0, then
            and #$e0            ;   mask away bit 4 for a zero-page mode
            tax                 ;   and stash it in X
            jsr SetMode         ; Test out zero-page mode and make sure that
            jsr OpLookup        ;   it's supported by this instruction;
            bcs abs_write       ;   if so, move along. Otherwise, bring us back
            txa                 ;   to the non-zero-page version (JMP, JSR)
            ora #$10            ;   by restoring bit 4
variant:    jsr SetMode
            jsr OpLookup
            bcs abs_write
            jmp AsmFail
abs_write:  ldy #$00
            sta (TARGET),y
            iny
            lda OPERAND
            sta (TARGET),y
            lda MNEMONIC+1      ; If this is a zero-page mode, don't write
            and #$10            ;   the third byte
            beq abs_r           ;   ,,
            iny
            lda OPERAND+1
            sta (TARGET),Y
abs_r:      jmp Return            

; Assemble Indirect
; Mostly the same as absolute, so this taps into AsmAbs at the appropriate
; point
AsmInd:     jsr CHRGET
            cmp #"$"
            beq ind_op
            jmp AsmFail
ind_op:     jsr GetOperand    
            lda #INDIRECT
            jmp modify

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; Get Operand
; Populate the operand for an instruction by looking forward in the buffer and
; counting upcoming hex digits.
GetOperand: tya
            pha
            ldy #$00            ; Valid number count
            sty OPERAND         ; Initialize operand
            sty OPERAND+1       ; ,,
            ldx BUFPTR          ; Save the buffer pointer for backtracking
-loop:      jsr CHRGET          ; Count the number of hexadecimal characters
            jsr Char2Nyb        ;   available in the buffer
            cmp #$10            ; Once we reach a non-hex character, the
            bcs counted         ;   count is over
            iny
            bne loop
counted:    stx BUFPTR          ; Backtrack the buffer pointer
            cpy #$02            ; Y can be 2 (one byte) or 4 (two bytes)
            beq found1          ; ,,
            cpy #$04            ; ,,
            beq found2          ; ,,
go_fail:    lda #$00            ; If any invalid number of hex digits were
            sta MNEMONIC        ;   provided, then corrupt the mnemonic to
            beq getop_r         ;   cause an error indicator
found2:     jsr Buff2Byte       ; Four characters were found; Put the byte value
            sta OPERAND+1       ;   of two in the high byte of the operand
found1:     jsr Buff2Byte       ; Get two characters for the operand low byte
            sta OPERAND         ; ,,
getop_r:    pla
            tay
            rts                                
            
; Buffer to Byte
; Y is the index of the first character of the byte in the text
; buffer, to be returned as a byte in the Accumulator
Buff2Byte:  jsr CHRGET
            jsr Char2Nyb        ; The first nybble at the index is
            asl                 ;   the high one, multipled by 16
            asl                 ;   ,,
            asl                 ;   ,,
            asl                 ;   ,,
            sta WORK
            jsr CHRGET          ; Get the next character, which is
            jsr Char2Nyb        ;   the low nybble, and combine the
            ora WORK            ;   nybbles
            rts
       
; Character to Nybble
; Y is the index of the character in the text buffer, to be converted
; into a nybble.
Char2Nyb:   cmp #$41            ; Is this a letter?
            bcs IsLetter
            sbc #$2f            ; If it's a number, subtract 48 ("0")
                                ; $2f is subtracted here because I know
                                ; that the carry flag is clear, and $2f
                                ; compensates for that
            rts
IsLetter:   sbc #$37            ; If it's a letter, subtract 55 from the
                                ; letter, because we want "F" (70) to return
                                ; 15 and "A" (65) to return 10, etc.
            rts 

; Get Alpha
; Get a letter, and return in A its ordinal number in the alphabet
GetAlpha:   jsr CHRGET
            sec
            sbc #$40
            rts      
                                                  
; Set Addressing Mode 
; The addressing mode is in the high nybble of the mnemonic encoding register     
SetMode:    pha                 ; Clear out the previous addressing
            lda MNEMONIC+1      ;   mode for this mnemonic, in case
            and #$0f            ;   we need to try a couple options
            sta MNEMONIC+1      ;   ,,
            pla                 ; Move the specified mode into high nybble
            ora MNEMONIC+1      ; And complete the update
            sta MNEMONIC+1      ; ,,
            rts    

; Loop up Opcode
; Returns the opcode in A
; Sets the Carry flag if an opcode was found, and clears the Carry flag
; if the search failed.
OpLookup:   lda #<LangTable
            sta WORK
            lda #>LangTable
            sta WORK+1
-loop:      ldy #$00
            lda (WORK),y
            cmp #$ff
            beq lu_fail
            cmp MNEMONIC
            bne next
            iny
            lda (WORK),y
            cmp MNEMONIC+1
            bne next
            iny 
            lda (WORK),y
            sec
            rts
next:       clc                 ; Advance three bytes into the table
            lda #$03            ; ,,
            adc WORK            ; ,,
            sta WORK            ; ,,
            bcc loop            ; ,,
            inc WORK+1          ; ,,
            bne loop            ; ,,
lu_fail:    clc
            rts

; Check Instruction for X or Y
; A starting addressing mode is provided in A. If the instruction contains ad
; comma followed by X or Y, then increase the addressing mode by 2 (for X) or
; 4 (for Y).            
CheckForXY: tay
            ldx BUFPTR          ; Save buffer, for when we need to backtrack
-loop:      jsr CHRGET
            cmp #QUOTE          ; Quote means the search is done
            beq check_done      ; ,,
            cmp #$00            ; As does 0
            beq check_done      ; ,,
            cmp #"X"            ; As does X, but modify the mode
            beq found_x         ; ,,
            cmp #"Y"            ; As does Y, but modify the mode
            beq found_y         ; ,,
            inx                 ; Save this buffer position and
            bne loop            ;   search again
found_y:    jsr ModMode2        ; Add $40 to mode if Y is found
found_x:    jsr ModMode2        ; Add $20 to mode if X is found
check_done: tya
            stx BUFPTR          ; Restore buffer to last position
            rts           

; Modify Mode by 2
; Add 2 to the addressing mode to modify for indexing. Used in conjunction
; with CheckForXY            
ModMode2:   tya
            clc
            adc #$20
            tay
            rts
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Each letter is given a coefficient, which is multiplied by the ordinal number
; of the letter in the alphabet:
;
; * The first letter's coefficient is 1
; * The second letter's coefficient is 32
; * The third letter's coefficient is 2. If the third character is X, add
;   an additional 1 to the encoding, to avoid encoding collisions.
;
LangTable:  .byte $87,$60,$69   ; ADC #oper
            .byte $87,$00,$65   ; ADC oper
            .byte $87,$20,$75   ; ADC oper,X
            .byte $87,$10,$6d   ; ADC oper
            .byte $87,$30,$7d   ; ADC oper,X
            .byte $87,$50,$79   ; ADC oper,Y
            .byte $87,$a0,$61   ; ADC (oper,X)
            .byte $87,$c0,$71   ; ADC (oper),Y
            .byte $c9,$61,$29   ; AND #oper
            .byte $c9,$01,$25   ; AND oper
            .byte $c9,$21,$35   ; AND oper,X
            .byte $c9,$11,$2d   ; AND oper
            .byte $c9,$31,$3d   ; AND oper,X
            .byte $c9,$51,$39   ; AND oper,Y
            .byte $c9,$a1,$21   ; AND (oper,X)
            .byte $c9,$c1,$31   ; AND (oper),Y
            .byte $79,$72,$0a   ; ASL A
            .byte $79,$02,$06   ; ASL oper
            .byte $79,$22,$16   ; ASL oper,X
            .byte $79,$12,$0e   ; ASL oper
            .byte $79,$32,$1e   ; ASL oper,X
            .byte $68,$b0,$90   ; BCC oper
            .byte $88,$b0,$b0   ; BCS oper
            .byte $c4,$b0,$f0   ; BEQ oper
            .byte $4a,$01,$24   ; BIT oper
            .byte $4a,$11,$2c   ; BIT oper
            .byte $b4,$b1,$30   ; BMI oper
            .byte $cc,$b1,$d0   ; BNE oper
            .byte $1a,$b2,$10   ; BPL oper
            .byte $58,$72,$00   ; BRK
            .byte $c8,$b2,$50   ; BVC oper
            .byte $89,$71,$18   ; CLC
            .byte $8b,$71,$d8   ; CLD
            .byte $95,$71,$58   ; CLI
            .byte $af,$71,$b8   ; CLV
            .byte $c3,$61,$c9   ; CMP #oper
            .byte $c3,$01,$c5   ; CMP oper
            .byte $c3,$21,$d5   ; CMP oper,X
            .byte $c3,$11,$cd   ; CMP oper
            .byte $c3,$31,$dd   ; CMP oper,X
            .byte $c3,$51,$d9   ; CMP oper,Y
            .byte $c3,$a1,$c1   ; CMP (oper,X)
            .byte $c3,$c1,$d1   ; CMP (oper),Y
            .byte $34,$62,$e0   ; CPX #oper
            .byte $34,$02,$e4   ; CPX oper
            .byte $34,$12,$ec   ; CPX oper
            .byte $35,$62,$c0   ; CPY #oper
            .byte $35,$02,$c4   ; CPY oper
            .byte $35,$12,$cc   ; CPY oper
            .byte $aa,$00,$c6   ; DEC oper
            .byte $aa,$20,$d6   ; DEC oper,X
            .byte $aa,$10,$ce   ; DEC oper
            .byte $aa,$30,$de   ; DEC oper,X
            .byte $d5,$70,$ca   ; DEX
            .byte $d6,$70,$88   ; DEY
            .byte $09,$62,$49   ; EOR #oper
            .byte $09,$02,$45   ; EOR oper
            .byte $09,$22,$55   ; EOR oper,X
            .byte $09,$12,$4d   ; EOR oper
            .byte $09,$32,$5d   ; EOR oper,X
            .byte $09,$52,$59   ; EOR oper,Y
            .byte $09,$a2,$41   ; EOR (oper,X)
            .byte $09,$c2,$51   ; EOR (oper),Y
            .byte $cf,$01,$e6   ; INC oper
            .byte $cf,$21,$f6   ; INC oper,X
            .byte $cf,$11,$ee   ; INC oper
            .byte $cf,$31,$fe   ; INC oper,X
            .byte $fa,$71,$e8   ; INX
            .byte $fb,$71,$c8   ; INY
            .byte $ca,$11,$4c   ; JMP oper
            .byte $ca,$91,$6c   ; JMP (oper)
            .byte $8e,$12,$20   ; JSR oper
            .byte $8e,$60,$a9   ; LDA #oper
            .byte $8e,$00,$a5   ; LDA oper
            .byte $8e,$20,$b5   ; LDA oper,X
            .byte $8e,$10,$ad   ; LDA oper
            .byte $8e,$30,$bd   ; LDA oper,X
            .byte $8e,$50,$b9   ; LDA oper,Y
            .byte $8e,$a0,$a1   ; LDA (oper,X)
            .byte $8e,$c0,$b1   ; LDA (oper),Y
            .byte $bd,$60,$a2   ; LDX #oper
            .byte $bd,$00,$a6   ; LDX oper
            .byte $bd,$40,$b6   ; LDX oper,Y
            .byte $bd,$10,$ae   ; LDX oper
            .byte $bd,$50,$be   ; LDX oper,Y
            .byte $be,$60,$a0   ; LDY #oper
            .byte $be,$00,$a4   ; LDY oper
            .byte $be,$20,$b4   ; LDY oper,X
            .byte $be,$10,$ac   ; LDY oper
            .byte $be,$30,$bc   ; LDY oper,X
            .byte $90,$72,$4a   ; LSR A
            .byte $90,$02,$46   ; LSR oper
            .byte $90,$22,$56   ; LSR oper,X
            .byte $90,$12,$4e   ; LSR oper
            .byte $90,$32,$5e   ; LSR oper,X
            .byte $0e,$72,$ea   ; NOP
            .byte $51,$62,$09   ; ORA #oper
            .byte $51,$02,$05   ; ORA oper
            .byte $51,$22,$15   ; ORA oper,X
            .byte $51,$12,$0d   ; ORA oper
            .byte $51,$32,$1d   ; ORA oper,X
            .byte $51,$52,$19   ; ORA oper,Y
            .byte $51,$a2,$01   ; ORA (oper,X)
            .byte $51,$c2,$11   ; ORA (oper),Y
            .byte $12,$71,$48   ; PHA
            .byte $30,$71,$08   ; PHP
            .byte $92,$71,$68   ; PLA
            .byte $b0,$71,$28   ; PLP
            .byte $0a,$72,$2a   ; ROL A
            .byte $0a,$02,$26   ; ROL oper
            .byte $0a,$22,$36   ; ROL oper,X
            .byte $0a,$12,$2e   ; ROL oper
            .byte $0a,$32,$3e   ; ROL oper,X
            .byte $16,$72,$6a   ; ROR A
            .byte $16,$02,$66   ; ROR oper
            .byte $16,$22,$76   ; ROR oper,X
            .byte $16,$12,$6e   ; ROR oper
            .byte $16,$32,$7e   ; ROR oper,X
            .byte $a4,$72,$40   ; RTI
            .byte $b8,$72,$60   ; RTS
            .byte $59,$60,$e9   ; SBC #oper
            .byte $59,$00,$e5   ; SBC oper
            .byte $59,$20,$f5   ; SBC oper,X
            .byte $59,$10,$ed   ; SBC oper
            .byte $59,$30,$fd   ; SBC oper,X
            .byte $59,$50,$f9   ; SBC oper,Y
            .byte $59,$a0,$e1   ; SBC (oper,X)
            .byte $59,$c0,$f1   ; SBC (oper),Y
            .byte $b9,$70,$38   ; SEC
            .byte $bb,$70,$f8   ; SED
            .byte $c5,$70,$78   ; SEI
            .byte $95,$02,$85   ; STA oper
            .byte $95,$22,$95   ; STA oper,X
            .byte $95,$12,$8d   ; STA oper
            .byte $95,$32,$9d   ; STA oper,X
            .byte $95,$52,$99   ; STA oper,Y
            .byte $95,$a2,$81   ; STA (oper,X)
            .byte $95,$c2,$91   ; STA (oper),Y
            .byte $c4,$02,$86   ; STX oper
            .byte $c4,$42,$96   ; STX oper,Y
            .byte $c4,$12,$8e   ; STX oper
            .byte $c5,$02,$84   ; STY oper
            .byte $c5,$22,$94   ; STY oper,X
            .byte $c5,$12,$8c   ; STY oper
            .byte $65,$70,$aa   ; TAX
            .byte $66,$70,$a8   ; TAY
            .byte $a5,$72,$ba   ; TSX
            .byte $16,$73,$8a   ; TXA
            .byte $3a,$73,$9a   ; TXS
            .byte $36,$73,$98   ; TYA
            .byte $ff           ; Not found

Intro:      .asc $0d,"WAX ON",$00
