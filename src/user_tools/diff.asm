; wAx Diff Tool
; 'start end target
; Compares bytes from start to end of range (inclusive) and reports
; each byte that does not match the same offset in target.

; wAx API
EFADDR      = $a6               ; Effective Address
X_PC        = $03               ; External Persistent Counter
RANGE_END   = $0254             ; End of range
Buff2Byte   = $7000             ; Get 8-bit hex number from input buffer to A
CharGet     = $7003             ; Get character from input buffer to A
CharOut     = $7006             ; Write character in A to output buffer
Hex         = $7009             ; Write value in A to output buffer 8-bit hex
IncAddr     = $700c             ; Increment Effective Address, store value in A
IncPC       = $700f             ; Increment Persistent Counter
Lookup      = $7012             ; Lookup 6502 instruction with operand in A
PrintBuff   = $7015             ; Flush output buffer to screen
ResetIn     = $7018             ; Reset input buffer index
ResetOut    = $701b             ; Reset output buffer index
ShowAddr    = $701e             ; Write Effective Address to output buffer
ShowPC      = $7021             ; Write Persistent Counter to output buffer

; Compare tool workspace
STATUS      = $0247             ; $00 = Non-Matching, $80 = Matching
COUNT       = $0248             ; Count of unmatched/matched bytes so far

*=$7900
Main:       bcc error           ; Error if the first address is no good
            jsr Buff2Byte       ; Get high byte of range end
            bcc error           ; ,,
            sta RANGE_END+1     ; ,,
            jsr Buff2Byte       ; Get low byte of range end
            bcc error           ; ,,
            sta RANGE_END       ; ,,
            jsr Buff2Byte       ; Get high byte of compare start
            bcc error           ; ,,
            sta X_PC+1          ; ,,
            jsr Buff2Byte       ; Get low byte of compare start
            bcc error           ; ,,
            sta X_PC            ; ,,
            lda #$00            ; Reset status and counter
            sta STATUS          ; ,,
            sta COUNT           ; ,,
            sta COUNT+1         ; ,,
            bcs Start           ; Setup OK, do compare
error:      jmp $cf08           ; ?SYNTAX ERROR, warm start

; Start comparison
Start:      jsr StartLine       ; Start address line
            inc RANGE_END       ; Increase the range by 1 for
            bne compare         ;   the comparison
            inc RANGE_END+1     ;   ,,
compare:    lda EFADDR+1        ; Is the effective address in 
            cmp RANGE_END+1     ;   the compare range?
            bcc in_range        ;   ,,
            lda EFADDR          ;   ,,
            cmp RANGE_END       ;   ,,
            bcc in_range        ; If so, check for byte match
done:       jsr Toggle          ; Toggle to show the right color
            jsr EndLine         ; Show the last result count
            rts                 ; Done!
in_range:   ldx #$00            ; Compare EA to PC
            lda (EFADDR,x)      ; ,,
            cmp (X_PC,x)        ; ,,
            bne differs
matches:    lda STATUS          ; If the byte matches, and the previous byte
            beq show_last       ;   differed, then toggle the status and reset
            bne next
differs:    lda STATUS          ; If the byte differs, and the previous byte
            bne show_last       ;   matched, then toggle the status and reset
            beq next
show_last:  jsr Toggle            
            jsr EndLine         ; Show the count
            lda #$00            ; Reset the counter
            sta COUNT           ; ,,
            sta COUNT+1         ; ,,
            jsr StartLine       ; Start a new line
next:       inc COUNT           ; Increment the count of match/unmatch
            bne inc_mem         ; ,,
            inc COUNT+1         ; ,,
inc_mem:    jsr IncPC           ; Increment the comparison counters
            jsr IncAddr         ; ,,
            jsr $ffe1           ; Check STOP key
            beq done            ; End the comparison if STOP
            jmp compare         ; Do the next comparison

Toggle:     lda STATUS
            bne toggle_off
            lda #$80
            .byte $34           ; DOP (skip byte)
toggle_off: asl
            sta STATUS
            rts

; Start Line
; Reset the output buffer, and add the addresses
StartLine:  jsr ResetOut
            lda #"$"
            jsr CharOut
            jsr ShowAddr
            lda #","
            jsr CharOut
            jsr ShowPC
            lda #":"
            jsr CharOut
            rts

; End Line
; Complete the line by showing the count in red (differences) or
; green (matches)            
EndLine:    lda COUNT           ; If the count is zero, don't
            bne report          ;   finish the buffer, because it's
            lda COUNT+1         ;   probably the first group of bytes
            bne report          ;   ,,
            rts                 ;   ,,
report:     lda $0286           ; Store current color
            pha                 ; ,,
            lda STATUS
            beq green
red:        lda #$1c            ; Red
            .byte $3c           ; TOP (skip word)
green:      lda #$1e            ; Green
            jsr CharOut
            lda COUNT+1         ; Show number of matches/no matches
            jsr Hex             ;   before the change
            lda COUNT           ;   ,,
            jsr Hex             ;   ,,
            jsr PrintBuff       ;   ,,
            pla                 ; Restore original color
            sta $0286           ; ,,
            rts
