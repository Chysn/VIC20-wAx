; SUBLEQ compiler plug-in for wAx
;
; 'addr aaaa bbbb cccc
; Where addr is the compile target address
;       aaaa is the address of A
;       bbbb is the address of B
;       cccc is the address of C
; 
;     Subtract A from B
;     Store result in B
;     Branch if B is less than or equal to zero
;
; References:
; https://en.wikipedia.org/wiki/One-instruction_set_computer
;    #Subtract_and_branch_if_less_than_or_equal_to_zero
; https://www.youtube.com/watch?v=FvwcRaE9yxc&t=142s

; wAx API used in this plug-in
EFADDR      = $a6               ; effective address
X_PC        = $03               ; External persistent counter
Buff2Byte   = $7000             ; Get 8-bit hex number from input buffer to A
CharOut     = $7006             ; Write character in A to output buffer
IncPC       = $700f             ; Increment persistent counter
ResetOut    = $701b             ; Reset output buffer index
EAtoPC      = $7024             ; Copy effective address to persistent counter
ShowPC      = $7021             ; Write persistent counter to output buffer
Store       = $0247             ; Plug-in storage (8 bytes)
OUTBUFFER   = $0218             ; Output buffer (24 bytes)

* = $033c

Main:       bcc subleq_r
            ldy #$00
-loop:      jsr EAtoPC
            jsr Buff2Byte       ; Get six hex bytes: AA BB CC
            bcc add_rts
            sta Store,y
            iny
            cpy #$06
            bne loop
            ldy #$00
-loop:      lda SubleqCode,y
            cmp #$f0            ; Bytes of $f and up are interpolated
            bcc go_on
            and #$0f            ; Put the index of the SUBLEQ operand
            tax                 ;   pointer in X
            lda Store,x         ; Get the data for the SUBLEQ instruction
go_on:      sta (EFADDR),y      ; Store the instruction in memory
            iny                 ; Get the next instruction from the table
            cpy #$11            ;   until done
            bne loop            ;   ,,
; After the SUBLEQ code is generated, show a prompt for the next instruction
            jsr EAtoPC          ; Update persistent counter
            tya                 ; $11
            clc                 ; Add 16 to PC for next address prompt
            adc X_PC            ; ,,
            sta X_PC            ; ,,
            bcc prompt          ; ,,
            inc X_PC+1          ; ,,
prompt:     jsr ResetOut        ; Reset output buffer
            lda #"'"            ; Show the plug-in character
            jsr CharOut         ; ,,
            jsr ShowPC          ; Show the next address
            lda #" "            ; Then a space
            jsr CharOut         ; ,,
            ldy #$00            ; Transfer everything to the keyboard
-loop:      lda OUTBUFFER,y     ;   buffer
            sta $0277,y         ;   ,,
            iny                 ;   ,,
            cpy #$06            ;   ,, (6 characters = "'addr ")
            bne loop            ;   ,,
            sty $c6             ; Store prompt in keyboard buffer
add_rts:    ldy #$00            ; Add an RTS if the next instruction isn't
            lda (X_PC),y        ;   LDA. If the next instruction IS LDA,
            cmp #$ad            ;   then this compiler assumes that the
            beq subleq_r        ;   current session is a patch of existing
            lda #$60            ;   code, and leaves the LDA alone
            sta (X_PC),y        ; Add RTS to end of program
            jsr IncPC           ; Increment counter
subleq_r:   rts
                                  
; This is the 6502 representation of a SUBLEQ instruction
; Bytes with high nybble $f are interpolated using the low
; nybble as the index to Store, with
;    $f1,$f0 = A
;    $f3,$f2 = B
;    $f5,$f4 = C
SubleqCode: .byte $ad,$f3,$f2   ; i0  lda b
            .byte $38           ;     sec
            .byte $ed,$f1,$f0   ;     sbc a
            .byte $8d,$f3,$f2   ;     sta b
            .byte $90,$02       ;     bcc br
            .byte $d0,$03       ;     bne i1
            .byte $4c,$f5,$f4   ; br  jmp c
