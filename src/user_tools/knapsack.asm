*=$a000

; BASIC Routines
ERROR       = $c437             ; Show error in X

; Knapsack Generator
KNAPSACK    = $033c             ; Knapsack storage (10 bytes)
BREAKPT     = KNAPSACK+10       ; Breakpoint address (2 bytes)
KNAPSIZE    = BREAKPT+2         ; Knapsack size (1 byte)
EFADDR      = $a6               ; Temp zeropage pointer to breakpoint address

; wAx Addresses
; To use this code for another system,
; (1) Remove the Install routine
; (2) Replace SizeOf with a routine that takes an opcode in A and returns
;     its size in X, and clears Carry if the opcode is unfound or a relative
;     branch instruction
Lookup      = $6a5a             ; Opcode lookup routine
ADDRMODE    = $0255             ; Addressing mode
USER_VECT   = $05               ; User tool vector

; Install for wAx user tool
Install:    lda #<Main
            sta USER_VECT
            lda #>Main
            sta USER_VECT+1
            rts

; Main routine entry point
; * If setting a breakpoint, its address is in EFADDR vector
; * If clearing a breakpoint, the Carry flag is clear
Main:       bcs NewKnap         ; A legal address has been provided in $a6/$a7
restore:    lda BREAKPT         ; Otherwise, restore the breakpoint to the
            sta EFADDR          ;   original code by copying the 3rd-5th
            lda BREAKPT+1       ;   bytes in the knapsack back to the code
            beq restore_r       ; If there's no knapsack installed, don't
            sta EFADDR+1        ;   uninstall one
            ldy KNAPSIZE        ;   ,,
            dey                 ;   ,,
-loop       lda KNAPSACK+2,y    ;   ,,
            sta (EFADDR),y      ;   ,,
            dey                 ;   ,,
            bpl loop            ;   ,,
            lda #$00            ; Reset the breakpoint so that doing it again
            sta BREAKPT+1       ;   doesn't mess up the original code
restore_r:  rts                 ;   ,,

; New Knapsack
; Generate a new knapsack at the effective address
NewKnap:    lda BREAKPT+1       ; Don't install a knapsack if there's already
            bne knap_r          ;   one installed
            ldy #$00            ; (BRK)
            sty KNAPSACK        ; ,,
            lda #$ea            ; (NOP)
            sta KNAPSACK+1      ; ,,
next_inst:  tya                 ; Preserve Y against SizeOf
            pha                 ; ,,
            lda (EFADDR),y      ; A = Opcode of the breakpoint instruction
            jsr SizeOf          ; X = Size of instruction (1-3)
            pla
            tay
            bcs xfer            ; Error if branch or unknown instruction
            ldx #$11            ; ?UNDEF'D STATEMENT ERROR     
            jmp ERROR           ; Show error, then warm start 
xfer:       lda (EFADDR),y      ; Move X bytes starting at Y index
            sta KNAPSACK+2,y    ; Y is a running count of knapsacked bytes
            iny                 ; ,,
            dex                 ; ,,
            bne xfer            ; ,,
            cpy #$03            ; If at least three bytes have been knapsacked
            bcc next_inst       ;   we're done
            lda EFADDR          ; Stack pointer in breakpoint storage for
            sta BREAKPT         ;   later restoration
            lda EFADDR+1        ;   ,,
            sta BREAKPT+1       ;   ,,
            sty KNAPSIZE        ; Save knapsack size for later
            lda #$ea            ; (NOP)
-loop:      cpy #$03            ; Pad code with more than three bytes with
            beq add_kjmp        ;   NOPs after the first three
            dey
            sta (EFADDR),y      ;   ,,
            bne loop            ;   ,,
add_kjmp:   ldy #$00
            lda #$4c            ; (JMP) This is the JMP to the knapsack
            sta (EFADDR),y      ; 
            lda #<KNAPSACK      ; Store knapsack JMP low byte
            iny                 ; ,,
            sta (EFADDR),y      ; ,,
            lda #>KNAPSACK      ; Store knapsack JMP high byte
            iny                 ; ,,
            sta (EFADDR),y      ; ,,
            lda KNAPSIZE        ; Calculate the return jump point (original
            tay                 ;   address + Y)
            clc                 ;   ,,
            adc EFADDR          ;   ,,
            sta KNAPSACK+3,y    ; Store return JMP low byte
            bcc add_rjmp        ;   ,,
            inc EFADDR+1        ;   ,,
add_rjmp:   lda #$4c            ; (JMP) This is the JMP to the return point        
            sta KNAPSACK+2,y    ; ,,
            lda EFADDR+1        ; Store return JMP high byte
            sta KNAPSACK+4,y    ; ,,
knap_r:     rts 
    
; Size Of Instruction
; Given an opcode in A, return instruction size in X and set Carry flag
; Carry clear indicates an error (known or relative branch instruction)        
SizeOf:     jsr Lookup
            bcs found
            rts                 ; Return with Carry clear to indicate error
found:      lda ADDRMODE        ; Addressing mode is in high nybble, so
            lsr                 ;   shift it to the right to get an index
            lsr                 ;   ,,
            lsr                 ;   ,,
            lsr                 ;   ,,
            tax                 ; Use that index to get the size from the
            lda AddrSize,x      ;   table
            bne mode_ok         ; If the addressing mode size is 0 in the table
            clc                 ;   clear Carry to indicate an illegal mode
            rts                 ;   ,,
mode_ok:    tax                 ; Otherwise, transfer the size to X and set
            sec                 ;   Carry flag to indicate success
            rts                 ;   ,,

; Size by addressing mode high nybble
AddrSize:   .byte 0,3,2,2,3,3,3,2,2,2,2,1,0,1
            