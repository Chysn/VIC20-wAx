; wAx Text Tool
; £[addr]
; List 16 characters per line, 16 lines at a time

; wAx API (partial)
CharOut     = $7006             ; Write character in A to output buffer
IncAddr     = $700c             ; Increment Effective Address, store value in A

*=$7a00
Main:       ldy #$00
-loop:      jsr IncAddr
            cmp #$a0            ; Everything from 160 on is allowed in the
            bcs add_char        ;   display unchaged
            cmp #$80            ; Change everything between 128 and 159 
            bcs alter_char      ; ,,
            cmp #$20            ; Show everything else at and above space
            bcs add_char        ; ,,
alter_char: lda #" "            ; Everything else gets a space
add_char:   jsr CharOut         ; ,,
            iny
            cpy #$10
            bne loop
            rts
