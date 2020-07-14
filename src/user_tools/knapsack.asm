*=$a000

; BASIC Routines
UND_ERROR   = $c8e3             ; UNDEF'D STATEMENT ERROR

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
            sta EFADDR          ;   original code by copying the
            lda BREAKPT+1       ;   bytes in the knapsack back to the code
            sta EFADDR+1        ;   ,,
            ldy KNAPSIZE        ; Get knapsack code size (3-5, or 0)
            beq restore_r       ; Don't restore if KNAPSIZE isn't set
            dey
-loop       lda KNAPSACK+2,y    ; Move between 3 and 5 bytes back
            sta (EFADDR),y      ;   to their original locations
            dey                 ;   ,,
            bpl loop            ;   ,,
            lda #$00            ; Reset the knapsack size so that doing it again
            sta KNAPSIZE        ;   doesn't mess up the original code
restore_r:  rts                 ;   ,,

; New Knapsack
; Generate a new knapsack at the effective address
NewKnap:    lda KNAPSIZE        ; Don't install a knapsack if there's already
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
            jmp UND_ERROR        ; ?UNDEF'D STATEMENT ERROR     
xfer:       lda (EFADDR),y      ; Move X bytes starting at Y index
            sta KNAPSACK+2,y    ; Y is a running count of knapsacked bytes
            iny                 ; ,,
            dex                 ; ,,
            bne xfer            ; ,,
            cpy #$03            ; If at least three bytes have been knapsacked
            bcc next_inst       ;   we're done
            lda EFADDR          ; Stash pointer in breakpoint storage for
            sta BREAKPT         ;   later restoration
            lda EFADDR+1        ;   ,,
            sta BREAKPT+1       ;   ,,
            sty KNAPSIZE        ; Save knapsack size for later
            lda #$ea            ; (NOP)
-loop:      cpy #$03            ; Pad code with more than three bytes with
            beq add_kjmp        ;   NOPs after the first three
            dey                 ;   ,,
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
            lda #$00
            adc EFADDR+1 
            sta KNAPSACK+4,y    ; Store return JMP high byte
            lda #$4c            ; (JMP) This is the JMP to the return point        
            sta KNAPSACK+2,y    ; ,,
knap_r:     rts 
    
; Size Of Instruction
; Given an opcode in A, return instruction size in X and set Carry flag
; Carry clear indicates an error (unknown or relative branch instruction)        
SizeOf:     jsr Lookup
            bcc size_r          ; Return with Carry clear to indicate error
            lsr                 ; Addressing mode is in high nybble, so
            lsr                 ;   shift it to the right to get an index
            lsr                 ;   ,,
            lsr                 ;   ,,
            tax                 ; Use that index to get the size from the
            lda AddrSize,x      ;   table
            beq size_r          ; Size zero is failure; Carry clear via LSR
            tax                 ; Return the size in X
            sec                 ; Set carry to indicate success
size_r:     rts            

; Size by addressing mode high nybble
AddrSize:   .byte 0,3,2,2,3,3,3,2,2,2,2,1,0,1


            