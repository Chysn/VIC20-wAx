;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                                      wAx
;                            Integrated Monitor Tools
;                             (c)2020, Jason Justian
;                  
; Release 1 - May 16, 2020
; Assembled with XA
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2020, Jason Justian
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; LABEL DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

* = $a000 

; Configuration
DISPLAYL    = $10               ; Display this many lines of code or memory
DCHAR       = "$"               ; Wedge character $ for disassembly
ACHAR       = "@"               ; Wedge character @ for assembly
MCHAR       = "&"               ; Wedge character & for memory dump
HCHAR       = ":"               ; Wedge character : for hex entry
BCHAR       = "!"               ; Wedge character ! for breakpoint
RCHAR       = ";"               ; Wedge character ; for register set
ECHAR       = $5f               ; Wedge character arrow for code execute

; System resources
IGONE       = $0308             ; Vector to GONE
CBINV       = $0316             ; BRK vector
GONE        = $c7e4
CHRGET      = $0073
BUF         = $0200             ; Input buffer
PRTSTR      = $cb1e             ; Print from data (Y,A)
SYS         = $e133             ; BASIC SYS start
CHROUT      = $ffd2
WARM_START  = $0302             ; BASIC warm start vector
READY       = $c002             ; BASIC warm start with READY.
NX_BASIC    = $c7ae             ; Get next BASIC command
BASICERR    = $c447             ; Basic error message
BUFPTR      = $7a               ; Pointer to buffer
CHARAC      = $07               ; Temporary character
CURLIN      = $39
KEYBUFF     = $0277             ; Keyboard buffer and size, for automatically
KBSIZE      = $c6               ;   advancing the assembly address
KEYCVTRS    = $028d             ; Keyboard codes
Acc         = $030c             ; Saved accumulator
XReg        = $030d             ; Saved X Register
YReg        = $030e             ; Saved Y Register
Proc        = $030f             ; Saved Processor Status
ERROR_PTR   = $22               ; BASIC error text pointer
SYS_DEST    = $14

; Constants
; Addressing mode encodings
INDIRECT    = $10               ; e.g., JMP ($0306)
INDIRECT_X  = $20               ; e.g., STA ($1E,X)
INDIRECT_Y  = $30               ; e.g., CMP ($55),Y
ABSOLUTE    = $40               ; e.g., JSR $FFD2
ABSOLUTE_X  = $50               ; e.g., STA $1E00,X
ABSOLUTE_Y  = $60               ; e.g., LDA $8000,Y
ZEROPAGE    = $70               ; e.g., BIT $A2
ZEROPAGE_X  = $80               ; e.g., CMP $00,X
ZEROPAGE_Y  = $90               ; e.g., LDX $FA,Y
IMMEDIATE   = $a0               ; e.g., LDA #$2D
IMPLIED     = $b0               ; e.g., INY
RELATIVE    = $c0               ; e.g., BCC $181E

; Other constants
TABLE_END   = $f2               ; Indicates the end of mnemonic table
QUOTE       = $22               ; Quote character

; Assembler workspace
WORK        = $a3               ; Temporary workspace (2 bytes)
PRGCTR      = $a5               ; Program Counter (2 bytes)
CHARDISP    = $a7               ; Character display for Memory (2 bytes)
LANG_PTR    = $a7               ; Language Pointer (2 bytes)
INSTDATA    = $a9               ; Instruction data (2 bytes)
FUNCTION    = $ab               ; Current function (ACHAR, DCHAR)
OPCODE      = $ac               ; Assembly target for hypotesting
OPERAND     = $ad               ; Operand storage (2 bytes)
RB_OPERAND  = $af               ; Hypothetical relative branch operand
CHRCOUNT    = $b0               ; Detokenization count
IDX_IN      = $b1               ; Buffer index
IDX_OUT     = $b2               ; Buffer index
OutBuffer   = $0218             ; Output buffer (24 bytes)
InBuffer    = $0230             ; Input buffer (22 bytes)
ZPTemp      = $0246             ; Zeropage Preservation (16 bytes)
Breakpoint  = $0256             ; Breakpoint data (3 bytes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; INSTALLER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
Install:    jsr $c533           ; Re-chain BASIC program to set BASIC
            lda $22             ;   pointers as a courtesy to the user
            adc #$02            ;   ,,
            sta $2D             ;   ,,
            lda $23             ;   ,,
            jsr $C655           ;   ,,
installed:  jsr SetupVec        ; Set up vectors (IGONE and BRK)
            lda #<Intro         ; Announce that wAx is on
            ldy #>Intro         ; ,,
            jsr PRTSTR          ; ,,            
            jmp (READY)
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; MAIN PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;              
main:       jsr CHRGET
            cmp #DCHAR          ; Disassembler
            beq Disp_Dasm       ; ,,
            cmp #ACHAR          ; Assembler
            beq Disp_Asm        ; ,,
            cmp #MCHAR          ; Memory Dump
            beq Disp_Mem        ; ,,
            cmp #HCHAR          ; Hex Editor
            beq Disp_Hex        ; ,,
            cmp #BCHAR          ; Breakpoint Manager
            beq Disp_BP         ; ,,
            cmp #RCHAR          ; Register Setter
            beq Disp_Reg        ; ,,
            cmp #ECHAR          ; Execute
            beq Disp_Exec       ; ,,
            jmp GONE+3          ; +3 because the CHRGET is already done
                        
; Dispatch Disassembler            
Disp_Dasm:  jsr Prepare
            jsr DisList
            jmp Return    

; Dispatch Assembler
Disp_Asm:   jsr Prepare
            jsr Assemble
            jmp Return
            
; Dispatch Memory Dump            
Disp_Mem:   jsr Prepare
            jsr Memory
            jmp Return  

; Dispatch Hex Editor            
Disp_Hex:   jsr Prepare
            jsr HexEditor
            jmp Return 
            
Disp_Reg:   jsr Prepare
            jsr Register
            jmp Return     
            
Disp_Exec:  jsr Prepare
            jmp Execute
                              
; Dispatch Breakpoint Manager
Disp_BP:    jsr Prepare
            jsr BPManager
            ; Falls through to Return
                        
; Return from Wedge
; Return in one of two ways:
; * In direct mode, to a BASIC warm start without READY.
; * In a program, back to GONE
Return:     jsr Restore
            ldy CURLIN+1        ; See if we're running in direct mode by
            iny                 ;   checking the current line number
            bne in_program      ;   ,,
            jmp (WARM_START)    ; If in direct mode, warm start without READY.            
in_program: jmp NX_BASIC        ; Otherwise, continue to next BASIC command          

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; DISASSEMBLER COMPONENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Disassembly Listing
; Disassemble multiple instructions, starting from the program counter
DisList:    ldx #DISPLAYL       ; Show this many lines of code
-loop:      txa
            pha
            jsr Disasm          ; Disassmble the code at the program counter
            jsr PrintBuff       ; Display the buffer
            lda #$92            ; Reverse off after each instruction
            jsr CHROUT          ; ,,
            lda #$0d            ; Carriage return after each instruction
            jsr CHROUT          ; ,,
            pla                 ; Restore iterator
            tax                 ; ,,
            dex
            bne loop
            inx
            jsr ShiftDown       ; Keep scrolling if the Shift Key
            bne loop            ;   is held down
            jsr EnableBP        ; Re-enable breakpoint, if necessary
            rts

; Disassemble
; Disassemble a single instruction at the program counter
Disasm:     lda #$00            ; Reset the buffer index
            sta IDX_OUT         ; ,,
            jsr BreakInd        ; Indicate breakpoint, if it's here
            lda FUNCTION        ; Start each line with the wedge character, so
            jsr CharOut         ;   the user can chain commands
            jsr Address
op_start:   ldy #$00            ; Get the opcode
            lda (PRGCTR),y      ;   ,,
            jsr Lookup          ; Look it up
            bcc Unknown         ; Clear carry indicates an unknown opcode
            jsr DMnemonic       ; Display mnemonic
            jsr Space
            jsr DOperand        ; Display operand
            lda #$00            ; Write $00 to the buffer for printing
disasm_r:   jsr CharOut         ;   purposes
            jsr NextValue       ; Advance to the next line of code
            rts

Unknown:    pha                 ; For an unknown opcode, show the hex
            jsr HexPrefix       ;   value at the location
            pla                 ;   ,,
            jsr Hex             ;   ,,
            lda #"?"            ;   ,,
            jmp disasm_r
            
; Write Mnemonic and Parameters
DMnemonic:  ldx INSTDATA        ; Get the index to the first two characters
            lda Tuplet,x        ;   of the mnemonic and write to buffer
            jsr CharOut         ;   ,,
            lda Tuplet+1,x      ;   ,,
            jsr CharOut         ;   ,,
            lda INSTDATA+1      ; Get the addressing mode
            and #$0f            ; ,,
            tax                 ; ,,
            lda Char3,x         ; Get the index to the third character of
            jsr CharOut         ;   the mnemonic and write to buffer
            rts

; Operand Display
; Dispatch display routines based on addressing mode
DOperand:   lda INSTDATA+1
            and #$f0            ; Isolate addressing mode from data table
            cmp #IMPLIED        ; Handle each addressing mode with a subroutine
            beq DisImp
            cmp #RELATIVE
            beq DisRel
            cmp #IMMEDIATE
            beq DisImm
            cmp #ZEROPAGE
            bcs DisZP
            cmp #ABSOLUTE
            bcs DisAbs
            ; Fall through to DisInd, because it's the only one left

; Disassemble Indirect Operand
DisInd:     pha
            lda #"("
            jsr CharOut
            pla
            cmp #INDIRECT
            bne ind_xy
            jsr Param_16
            lda #")"
            jsr CharOut
            rts
ind_xy:     pha
            jsr Param_8
            pla
            cmp #INDIRECT_X
            bne ind_y
            lda #","
            jsr CharOut
            lda #"X"
            jsr CharOut
            lda #")"
            jsr CharOut
            rts
ind_y:      lda #")"
            jsr CharOut
            lda #","
            jsr CharOut
            lda #"Y"
            jsr CharOut
DisImp:     rts                 ; Any convenient rts will do for Implied     

; Disassemble Immediate Operand         
DisImm:     lda #"#"
            jsr CharOut
            jsr Param_8
            rts

DisZP:      pha
            jsr Param_8
            pla
            sec
            sbc #ZEROPAGE
            jmp draw_xy         ; From this point, it's the same as Absolute            

DisRel:     jsr HexPrefix
            jsr NextValue       ; Get the operand of the instruction, advance
                                ;   the program counter. It might seem weird to
                                ;   advance the PC when we're operating on it a
                                ;   few lines down, but we need to add two
                                ;   bytes to get the offset to the right spot.
                                ;   One of those bytes is here, and the other
                                ;   comes from setting the Carry flag before
                                ;   the addition below
            sta WORK
            and #$80            ; Get the sign of the operand
            beq sign
            ora #$ff            ; Extend the sign out to 16 bits, if negative
sign:       sta WORK+1          ; Set the high byte to either $00 or $ff
            lda WORK
            sec                 ; sec here before adc is not a mistake; I need
            adc PRGCTR          ;   to account for the instruction address
            sta WORK            ;   (see above)
            lda WORK+1          ;
            adc PRGCTR+1        ;
            jsr Hex             ; No need to save the high byte, just show it
            lda WORK            ; Show the low byte of the computed address
            jsr Hex             ; ,,
            rts
                            
; Disassemble Absolute Operand           
DisAbs:     pha                 ; Save addressing mode for use later
            jsr Param_16
            pla
            sec
            sbc #ABSOLUTE
draw_xy:    ldx #"X"
            cmp #$10
            beq abs_ind
            ldx #"Y"
            cmp #$20
            beq abs_ind
            rts
abs_ind:    lda #","            ; This is an indexed addressing mode, so
            jsr CharOut         ;   write a comma and index register
            txa                 ;   ,,
            jsr CharOut         ;   ,,
            rts                      
                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; ASSEMBLER COMPONENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Assemble:   jsr CharGet         ; Look through the buffer for one of two things
            cmp #"$"            ;   A $ indicates there's an operand. We need to
            beq get_oprd        ;   parse that operand, or
            cmp #$00            ;   If we reach the end of the buffer, there's
            beq test            ;   no operand, so go to test the instruction
            bne Assemble        ;   ,,
get_oprd:   jsr GetOperand      ; Once $ is found, then grab the operand
test:       lda IDX_IN          ; If not enough characters have been entered to
            cmp #$06            ;   be mistaken for an intentional instrution,
            bcc asm_r           ;   just go to BASIC
-loop:      jsr Hypotest        ; Line is done; hypothesis test for a match
            bcc AsmFail         ; Clear carry means the test failed
            ldy #$00            ; A match was found! Transcribe the good code
            lda OPCODE          ;   to the program counter. The number of bytes
            sta (PRGCTR),y      ;   to transcribe is stored in the CHRCOUNT memory
            ldx CHRCOUNT        ;   location.
            cpx #$02            ; Store the low operand byte, if indicated
            bcc nextline        ; ,,
            lda OPERAND         ; ,,
            iny                 ; ,,
            sta (PRGCTR),y      ; ,,
            cpx #$03            ; Store the high operand byte, if indicated
            bcc nextline        ; ,,
            lda OPERAND+1       ; ,,
            iny                 ; ,,
            sta (PRGCTR),y      ; ,,
nextline:   jsr ClearBP         ; Clear breakpoint on successful assembly
            jsr Prompt          ; Prompt for next line if in direct mode
asm_r:      rts
            
; Assembly Fail
; Invalid opcode or formatting
AsmFail:    jsr Restore
            lda #<Error
            sta ERROR_PTR
            ldy #>Error  
            sty ERROR_PTR+1
            jmp BASICERR

; Get Operand
; Populate the operand for an instruction by looking forward in the buffer and
; counting upcoming hex digits.
GetOperand: jsr Buff2Byte       ; Get the first byte
            bcc getop_r         ; If invalid, return
            sta OPERAND+1       ; Default to being high byte
            jsr Buff2Byte
            bcc mov_low         ; Only 8-bit operand provided
            sta OPERAND         ; Have 16-bits, so set low byte
            sec                 ; Compute hypothetical relative branch
            sbc PRGCTR          ; Subtract the program counter address from
            sec                 ;   the instruction target
            sbc #$02            ; Offset by 2 to account for the instruction
            sta RB_OPERAND      ; Save the hypothetical relative branch operand
getop_r:    rts
mov_low:    lda OPERAND+1       ; It's an 8-bit operand, so the first value
            sta OPERAND         ;   provided is moved to the low byte
            rts
            
; Hypothesis Test
; Search through
Hypotest:   lda PRGCTR+1        ; Save the program counter from the assembler
            pha                 ;   so it can be used by the disassembler
            lda PRGCTR
            pha
            jsr ResetLang       ; Reset language table
reset:      lda #OPCODE         ; Write location to PC for hypotesting
            sta PRGCTR          ; ,,
            ldy #$00            ; Set the program counter high byte
            sty PRGCTR+1        ; ,,
            lda (LANG_PTR),y    ; A is this language entry's opcode
            cmp #TABLE_END      ; If the table has ended, leave the
            beq bad_code        ;   hypotesting routine
            sta OPCODE          ; Store it in the hypotesting location
            jsr Disasm          ; Disassemble using the opcode
            lda INSTDATA+1      ; This is a relative branch instruction, and
            and #$f0            ;   these are handled differently. See below
            cmp #RELATIVE       ;   ,,
            beq test_rel        ;   ,,
            ldy #$00
-loop:      lda InBuffer+4,y    ; Compare the assembly with the disassembly
            cmp OutBuffer+5,y   ;   in the buffers
            bne differ          ; If any bytes don't match, then quit
            iny
            cmp #$00
            bne loop            ; Loop until the buffer is done
match:      lda PRGCTR          ; Set the CHRCOUNT location to the number of
            sec                 ;   bytes that need to be programmed
            sbc #OPCODE         ;   ,,
            sta CHRCOUNT        ;   ,,
            pla                 ; Restore the program counter so that the
            sta PRGCTR          ;   instruction is loaded to the right place
            pla                 ;   ,,
            sta PRGCTR+1        ;   ,,
            sec                 ; Set Carry flag to indicate success
            rts
differ:     jsr AdvLang         ; Advance the counter
            jmp reset
test_rel:   ldy #$03            ; Here, relative branching instructions are
-loop:      lda OutBuffer+5,y   ;   handled. Only the first four characters
            cmp InBuffer+4,y    ;   are compared. If there's a match on the
            bne differ          ;   mnemonic + $, then move the computed
            dey                 ;   relative operand into the regular operand
            bpl loop            ;   low byte, and then treat this as a regular
            lda RB_OPERAND      ;   match after that
            sta OPERAND         ;   ,,
            jmp match           ;   ,,
bad_code:   pla                 ; Pull the program counter off the stack, but
            pla                 ;   there's no need to do anything with it
            clc                 ;   because we're giving up.
            rts
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; MEMORY DUMP COMPONENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Memory:     ldx #DISPLAYL       ; Show this many groups of four
-next:      txa
            pha
            lda #$00
            sta IDX_OUT
            lda #MCHAR          ; Start each line with the wedge character, so
            jsr CharOut         ;   the user can chain commands
            jsr Address
            ldy #$00
-loop:      lda (PRGCTR),y
            sta CHARDISP,y
            jsr Hex
            iny
            cpy #$04
            beq show_char
            jsr Space
            jmp loop       
show_char:  lda #$12            ; Reverse on for the characters
            jsr CharOut
            ldy #$00
-loop:      lda CHARDISP,y
            and #$7f            ; Mask off the high bit for character display;
            cmp #QUOTE          ; Don't show double quotes
            beq alter_char      ; ,,
            cmp #$20            ; Show everything else at and above space
            bcs add_char        ; ,,
alter_char: lda #$2e            ; Everything else gets a .
add_char:   jsr CharOut         ; ,,
            iny
            cpy #04
            bne loop            
            tya                 ; Advance the program counter by four bytes
            clc                 ;   for the next line of memory values
            adc PRGCTR          ;   ,,
            sta PRGCTR          ;   ,,
            bcc rev_off         ;   ,,
            inc PRGCTR+1        ;   ,,
rev_off:    lda #$92            ; Reverse off after the characters
            jsr CHROUT          ; ,,
            lda #$0d
            jsr CharOut
            jsr PrintBuff       ; Display the buffer
            pla                 ; Restore iterator
            tax                 ; ,,
            dex
            bne next
            inx
            jsr ShiftDown       ; Keep scrolling if the Shift Key
            bne next            ;   is held down
            rts
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; HEX EDITOR COMPONENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
HexEditor:  ldy #$00
-loop:      jsr Buff2Byte
            bcc hex_exit        ; Bail out on the first non-hex byte
            sta (PRGCTR),y      
            iny
            cpy #$04
            bne loop
hex_exit:   cpy #$00
            beq hex_r
            tya
            tax
            jsr Prompt          ; Prompt for the next address
            jsr ClearBP         ; Clear breakpoint if anything was changed
hex_r:      rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; BREAKPOINT COMPONENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
BPManager:  php
            jsr ClearBP         ; Clear the old breakpoint, if it exists
            plp                 ; If no breakpoint is chosen (e.g., if ! was)
            bcc bpm_r           ;   by itself), just clear the breakpoint
            lda PRGCTR          ; Add a new breakpoint at the program counter
            sta Breakpoint      ; ,,
            lda PRGCTR+1        ; ,,
            sta Breakpoint+1    ; ,,
            ldy #$00            ; Get the previous code
            lda (PRGCTR),y      ; Stash it in the Breakpoint data structure,
            sta Breakpoint+2    ;   to be restored on the next break
            lda #$00            ; Write BRK to the breakpoint location
            sta (PRGCTR),y      ;   ,,
            jsr Disasm          ; Disassemble the line at the breakpoint
            lda #$91            ;   for the user to review
            jsr CHROUT          ;   ,,
            jsr PrintBuff       ;   ,,
            lda #$0d            ;   ,,
            jsr CHROUT          ;   ,,
            jsr EnableBP        ; Enable the breakpoint after disassembly
bpm_r:      jsr SetupVec        ; Make sure that the BRK handler is on
            rts

Break:      cld                 ; Escape hatch for accidenally-set Decimal flag
            lda #$00
            sta IDX_OUT
            lda #<Registers     ; Print register indicator bar
            ldy #>Registers     ; ,,
            jsr PRTSTR          ; ,,
            ldy #$04            ; Pull four values off the stack and add
-loop:      pla                 ;   each one to the buffer. These values came
            jsr Hex             ;   from the hardware IRQ, and are Y,X,A,P.
            jsr Space           ;   ,,
            dey                 ;   ,,
            bne loop            ;   ,,
            tsx                 ; Stack pointer
            txa                 ; ,,
            jsr Hex             ; ,,
            jsr Space           ; ,,
            pla                 ; Program counter low
            tay
            pla                 ; Program counter high
            jsr Hex             ; High to buffer
            tya                 ; ,, 
            jsr Hex             ; Low to buffer with no space
            jsr PrintBuff       ; Print the buffer
            jsr ClearBP         ; Reset the Breakpoint data
            lda #$0d            ; Drop to the next line
            jsr CHROUT          ; ,,
            jmp (WARM_START)    
            
ClearBP:    lda Breakpoint+2    ; Is there an existing breakpoint?
            beq cleared         ; If not, do nothing
            lda Breakpoint      ; Get the breakpoint
            sta CHARAC          ; Stash it in a zeropage location
            lda Breakpoint+1    ; ,,
            sta CHARAC+1        ; ,,
            ldy #$00
            lda (CHARAC),y      ; What's currently at the Breakpoint?
            bne bp_reset        ; If it's not a BRK, then preserve what's there
            lda Breakpoint+2    ; Otherwise, get the breakpoint byte and
            sta (CHARAC),y      ;   put it back 
bp_reset:   sty Breakpoint      ; And then clear out the whole
            sty Breakpoint+1    ;   breakpoint data structure
            sty Breakpoint+2    ;   ,,
cleared:    rts

; Breakpoint Indicator
; Also restores the breakpoint byte, temporarily
BreakInd:   ldy #$00            ; Is this a BRK instruction?
            lda (PRGCTR),y      ; ,,
            bne ind_r           ; If not, do nothing
            lda Breakpoint      ; If it is a BRK, is it our breakpoint?
            cmp PRGCTR          ; ,,
            bne ind_r           ; ,,
            lda Breakpoint+1    ; ,,
            cmp PRGCTR+1        ; ,,
            bne ind_r           ; ,,
            lda #$12            ; Reverse on for the breakpoint
            jsr CharOut
            lda Breakpoint+2    ; Temporarily restore the breakpoint byte
            sta (PRGCTR),y      ;   for disassembly purposes
ind_r:      rts        
                 
; Enable Breakpoint
; Used after disassembly, in case the BreakInd turned the breakpoint off
EnableBP:   lda Breakpoint+2
            beq enable_r
            lda Breakpoint
            sta CHARAC
            lda Breakpoint+1
            sta CHARAC+1
            ldy #$00            ; Write BRK to the breakpoint
            lda #$00            ; ,,
            sta (CHARAC),y      ; ,,
enable_r:   rts
             
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; REGISTER COMPONENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Register:   lda PRGCTR+1        ; Two bytes are already set in the program
            sta YReg            ;   counter. These are Y and X
            lda PRGCTR          ;   ,,
            sta XReg            ;   ,,
            jsr Buff2Byte       ; Get a third byte to set Accumulator
            sta Acc             ;   ,,
            jsr Buff2Byte       ; Get a fourth byte to set Processor Status
            sta Proc            ;   ,,
            rts
                                                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; EXECUTE COMPONENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Execute:    jsr SetupVec        ; Make sure the BRK handler is enabled
            bcc ex_r            ; BRK if the provided address was no good
            lda PRGCTR          ; Set the temporary INT storage to the program
            sta SYS_DEST        ;   counter. This is what SYS uses for its
            lda PRGCTR+1        ;   execution address, and we're using that
            sta SYS_DEST+1      ;   system.
            jsr Restore         ; Restore the zeropage locations used
            jsr SYS             ; Call BASIC SYS from where it pushes RTS values
ex_r:       brk                 ; Trigger the BRK handler

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; Prepare
; (1) Save registers for Return
; (2) Save the current Function
; (3) Get the program counter address from the input
Prepare:    tay                 ; Y = the wedge character for function dispatch
            ldx #$00            ; wAx is to be zeropage-neutral, so preserve
-loop:      lda WORK,x          ;   its workspace in temp storage. When this
            sta ZPTemp,x        ;   routine is done, the data will be restored
            inx                 ;   in Return
            cpx #$10            ;   ,,
            bne loop            ;   ,,
            sty FUNCTION        ; Store the mode (to normalize spaces in buffer)
            lda #$00            ; Initialize the input index for write
            sta IDX_IN          ; ,,
            jsr Transcribe      ; Transcribe from CHRGET to InBuffer
            sta IDX_IN          ; Re-initialize for buffer read
            jsr Buff2Byte       ; Convert 2 characters to a byte   
            php                 ; Use this byte to determine success         
            sta PRGCTR+1        ; Save to the PRGCTR high byte
            jsr Buff2Byte       ; Convert next 2 characters to byte
            sta PRGCTR          ; Save to the PRGCTR low byte
            plp
            rts
; Restore
; Put back temporary zeropage workspace            
Restore:    ldx #$00            ; Restore workspace memory to zeropage
-loop:      lda ZPTemp,x        ;   ,,
            sta WORK,x          ;   ,,
            inx                 ;   ,,
            cpx #$10            ;   ,,
            bne loop            ;   ,,
            rts            

; Look Up Opcode             
Lookup:     sta INSTDATA        ; Store the requested opcode for lookup
            jsr ResetLang       ; Reset language table
-loop:      ldy #$00            ; Look at the first of three bytes in a table
            lda (LANG_PTR),y
            cmp #TABLE_END
            beq not_found
            cmp INSTDATA
            beq found
            jsr AdvLang         ; Not found; advance to next entry and look
            bne loop            ;   again
not_found:  lda INSTDATA        ; Opcode not found; clear Carry flag to indicate
            clc                 ;   unknown opcode, and set A back to the
            rts                 ;   original byte
found:      iny                 ; The opcode has been found; store the
            lda (LANG_PTR),y    ;   mnemonic and addressing mode information
            sta INSTDATA        ;   to draw the instruction
            iny                 ;   ,,
            lda (LANG_PTR),y    ;   ,,
            sta INSTDATA+1      ;   ,,
            sec                 ; Set Carry flag to indicate successful lookup
            rts  
                        
; Reset Language Table            
ResetLang:  lda #<Instr6502
            sta LANG_PTR
            lda #>Instr6502
            sta LANG_PTR+1
            rts
            
; Advance Language Table
; to next entry
AdvLang:    lda #$03            ; Each language entry is three bytes
            clc
            adc LANG_PTR
            sta LANG_PTR
            lda #$00
            adc LANG_PTR+1
            sta LANG_PTR+1 
            rts
            
; Get Character
; Akin to CHRGET, but scans the InBuffer, which has already been detokenized            
CharGet:    ldx IDX_IN
            lda InBuffer,x
            inc IDX_IN
            rts             
            
; Buffer to Byte
; Y is the index of the first character of the byte in the input
; buffer, to be returned as a byte in the Accumulator
Buff2Byte:  jsr CharGet
            jsr Char2Nyb
            cmp #TABLE_END
            beq byte_err
            asl                 ; Multiple high nybble by 16
            asl                 ;   ,,
            asl                 ;   ,,
            asl                 ;   ,,
            sta WORK
            jsr CharGet
            jsr Char2Nyb
            cmp #TABLE_END
            beq byte_err
            ora WORK            ; Combine high and low nybbles
            sec                 ; Set Carry flag indicates success
            rts
byte_err:   clc
            rts
       
; Character to Nybble
; A is the character in the text buffer to be converted into a nybble
Char2Nyb:   ldx #$0f            ; Iterate through the HexDigit table
-loop:      cmp HexDigit,x      ;   until the specified character is found
            beq found_dig       ;   ,,
            dex                 ;   ,,
            bpl loop            ;   ,,
            ldx #TABLE_END      ; If it's not found, set an error value
found_dig:  txa
            rts            

; Next Program Counter
; Advance Program Counter by one byte, and return its value
NextValue:  inc PRGCTR
            bne next_r
            inc PRGCTR+1
next_r:     ldy #$00
            lda (PRGCTR),y
            rts

; Show Hex Prefix
HexPrefix:  lda #"$"
            jsr CharOut
            rts

; Show Space           
Space:      lda #" "
            jsr CharOut
            rts
            
; Write Address to Buffer            
Address:    lda PRGCTR+1        ; Show the address
            jsr Hex             ; ,,
            lda PRGCTR          ; ,,
            jsr Hex             ; ,,
            jsr Space
            rts

; Show Hex Byte
Hex:        pha                 ; Show the high nybble first
            lsr
            lsr
            lsr
            lsr
            tax
            lda HexDigit,x
            jsr CharOut
            pla
            and #$0f
            tax
            lda HexDigit,x
            jsr CharOut
            rts
 
; Show 8-bit Parameter           
Param_8:    jsr HexPrefix
            jsr NextValue 
            jsr Hex            
            rts
            
; Show 16-Bit Parameter            
Param_16:   jsr HexPrefix
            jsr NextValue 
            pha
            jsr NextValue 
            jsr Hex
            pla
            jsr Hex
            rts
            
CharOut:    sta CHARAC          ; Save temporary character
            lda #ACHAR          ; If wAx is in Assembler mode, then
            cmp FUNCTION        ;   ignore spaces in the buffer
            bne write_ok        ;   ,,
            lda #" "            ;   ,,
            cmp CHARAC          ;   ,,
            beq write_r         ;   ,,
write_ok:   tya                 ; Save registers
            pha                 ; ,,
            txa                 ; ,,
            pha                 ; ,,
            ldx IDX_OUT         ; Write to the next OutBuffer location
            lda CHARAC          ; ,,
            sta OutBuffer,x     ; ,,
            inc IDX_OUT         ; ,,
            pla                 ; Restore registers
            tax                 ; ,,
            pla                 ; ,,
            tay                 ; ,,
write_r:    rts 

; Transcribe to Buffer
; Get a character from the input buffer and transcribe it to the
; input buffer. If the character is a BASIC token, then possibly
; explode it into individual characters.
Transcribe: jsr CHRGET
            cmp #$00
            beq xscribe_r
            cmp #$80
            bcs Detokenize
x_add:      jsr AddInput
            jmp Transcribe
xscribe_r:  jsr AddInput        ; Add the final zero
            rts

; Add Input
; Add a character to the input buffer and advance the counter
AddInput:   ldx IDX_IN
            sta InBuffer,x
            inc IDX_IN
            rts
           
; Detokenize
; If one of a specific set of tokens (AND, OR, DEF) is found, explode that
; token into PETSCII characters so it can be disassembled
Detokenize: ldy #$00            ; Iterate through the token table looking
-loop:      cmp Token,y         ;   for the possible token
            beq explode         ; Found the token, so explode it
            iny
            cpy #$0f
            bne loop
            jmp Transcribe      ; Ignore invalid tokens and move on
explode:    lda #$03
            sta CHRCOUNT
get_next:   iny
            lda Token,y         ; Character from the table
            beq detoken_r
            jsr AddInput        ; Add it to input buffer
            dec CHRCOUNT
            bne get_next
detoken_r: jmp Transcribe
 
; Print Buffer
; Add a $00 delimiter to the end of the output buffer, and print it out           
PrintBuff:  lda #$00            ; End the buffer with 0
            jsr CharOut         ; ,,
            lda #<OutBuffer     ; Print the line
            ldy #>OutBuffer     ; ,,
            jsr PRTSTR          ; ,,
            rts 
            
; Prompt for Next Line
; X should be set to the number of bytes the program counter should be
; advanced
Prompt:     ldy CURLIN+1        ; If an editor command is performed in
            iny                 ;   direct mode, then advance the program
            bne prompt_r        ;   counter by the size of the instruction
            tya                 ;   and write it to the keyboard buffer (by
            sta IDX_OUT         ;   way of populating the output buffer)
            lda FUNCTION        ;   ,,
            jsr CharOut         ;   ,,
            txa                 ;   ,,
            clc                 ;   ,,
            adc PRGCTR          ;   ,,
            sta PRGCTR          ;   ,,
            tya                 ;   ,, (Y is still $00, otherwise lda #$00)
            adc PRGCTR+1        ;   ,,
            jsr Hex             ;   ,,
            lda PRGCTR          ;   ,,
            jsr Hex             ;   ,,
-loop:      lda OutBuffer,y     ; Copy the output buffer into KEYBUFF, which
            sta KEYBUFF,y       ;   will simulate user entry
            iny                 ;   ,,
            cpy #$05            ;   ,,
            bne loop            ;   ,,
            lda #$20            ; Add the space to the keyboard buffer this way
            sta KEYBUFF,y       ;   because spaces are disabled for the
            iny                 ;   assembler
            sty KBSIZE          ; Setting the buffer size will make it go
prompt_r:   rts            
                
; Set Up Vectors
; Used by installation, and also by the breakpoint manager                    
SetupVec:   lda #<main          ; Intercept GONE to process wedge
            sta IGONE           ;   commands
            lda #>main          ;   ,,
            sta IGONE+1         ;   ,,
            lda #<Break         ; Set the BRK interrupt vector
            sta CBINV           ; ,,
            lda #>Break         ; ,,
            sta CBINV+1         ; ,,
            rts
            
; Check Shift Key
; If it's held down, Zero flag will be clear
ShiftDown:  lda KEYCVTRS   
            and #$01
            rts         
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Token table is used to detokenize certain instructions that might
; appear in assembly or addresses
Token:      .byte $96,$44,$45,$46   ; DEF
            .byte $af,$41,$4e,$44   ; AND
            .byte $b0,$4f,$52,$00   ; OR

; Miscellaneous data tables
HexDigit:   .asc "0123456789ABCDEF"
Intro:      .asc $0d,"WAX ON",$00
Error:      .asc "ASSEMBL",$d9
Registers:  .asc $0d,"BRK",$0d," Y: X: A: P: S: PC::",$0d,";",$00

; Tuplet and Char3 are used to decode instruction names. Tuplet should be padded
; to 64 characters, and Char3 should be padded to 16 characters, to support
; language table extensions.           
Tuplet:     .asc "ASEORTANOADEBPLSBMTXCMCPHBCLDBNJMTSTYBINBEBVBROJS"
Padding1:   .asc "---------------"
Char3:      .asc "ACDEIKLPQRSTVXY"
Padding2:   .asc "-"

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
Instr6502:  .byte $ea,$07,$b7   ; NOP
            .byte $60,$04,$ba   ; RTS
            .byte $a9,$1b,$a0   ; LDA #oper
            .byte $a5,$1b,$70   ; LDA oper
            .byte $b5,$1b,$80   ; LDA oper,X
            .byte $ad,$1b,$40   ; LDA oper
            .byte $bd,$1b,$50   ; LDA oper,X
            .byte $b9,$1b,$60   ; LDA oper,Y
            .byte $a1,$1b,$20   ; LDA (oper,X)
            .byte $b1,$1b,$30   ; LDA (oper),Y
            .byte $4c,$1f,$47   ; JMP oper
            .byte $6c,$1f,$17   ; JMP (oper)
            .byte $20,$2f,$49   ; JSR oper
            .byte $a2,$1b,$ad   ; LDX #oper
            .byte $a6,$1b,$7d   ; LDX oper
            .byte $b6,$1b,$9d   ; LDX oper,Y
            .byte $ae,$1b,$4d   ; LDX oper
            .byte $be,$1b,$6d   ; LDX oper,Y
            .byte $a0,$1b,$ae   ; LDY #oper
            .byte $a4,$1b,$7e   ; LDY oper
            .byte $b4,$1b,$8e   ; LDY oper,X
            .byte $ac,$1b,$4e   ; LDY oper
            .byte $bc,$1b,$5e   ; LDY oper,X
            .byte $c9,$14,$a7   ; CMP #oper
            .byte $c5,$14,$77   ; CMP oper
            .byte $d5,$14,$87   ; CMP oper,X
            .byte $cd,$14,$47   ; CMP oper
            .byte $dd,$14,$57   ; CMP oper,X
            .byte $d9,$14,$67   ; CMP oper,Y
            .byte $c1,$14,$27   ; CMP (oper,X)
            .byte $d1,$14,$37   ; CMP (oper),Y
            .byte $e0,$16,$ad   ; CPX #oper
            .byte $e4,$16,$7d   ; CPX oper
            .byte $ec,$16,$4d   ; CPX oper
            .byte $c0,$16,$ae   ; CPY #oper
            .byte $c4,$16,$7e   ; CPY oper
            .byte $cc,$16,$4e   ; CPY oper
            .byte $90,$19,$c1   ; BCC oper
            .byte $b0,$19,$ca   ; BCS oper
            .byte $f0,$28,$c8   ; BEQ oper
            .byte $d0,$1d,$c3   ; BNE oper
            .byte $ca,$0a,$bd   ; DEX
            .byte $88,$0a,$be   ; DEY
            .byte $e8,$26,$bd   ; INX
            .byte $c8,$26,$be   ; INY
            .byte $85,$22,$70   ; STA oper
            .byte $95,$22,$80   ; STA oper,X
            .byte $8d,$22,$40   ; STA oper
            .byte $9d,$22,$50   ; STA oper,X
            .byte $99,$22,$60   ; STA oper,Y
            .byte $81,$22,$20   ; STA (oper,X)
            .byte $91,$22,$30   ; STA (oper),Y
            .byte $86,$22,$7d   ; STX oper
            .byte $96,$22,$9d   ; STX oper,Y
            .byte $8e,$22,$4d   ; STX oper
            .byte $84,$22,$7e   ; STY oper
            .byte $94,$22,$8e   ; STY oper,X
            .byte $8c,$22,$4e   ; STY oper
            .byte $aa,$05,$bd   ; TAX
            .byte $a8,$05,$be   ; TAY
            .byte $ba,$21,$bd   ; TSX
            .byte $8a,$12,$b0   ; TXA
            .byte $9a,$12,$ba   ; TXS
            .byte $98,$23,$b0   ; TYA
            .byte $69,$09,$a1   ; ADC #oper
            .byte $65,$09,$71   ; ADC oper
            .byte $75,$09,$81   ; ADC oper,X
            .byte $6d,$09,$41   ; ADC oper
            .byte $7d,$09,$51   ; ADC oper,X
            .byte $79,$09,$61   ; ADC oper,Y
            .byte $61,$09,$21   ; ADC (oper,X)
            .byte $71,$09,$31   ; ADC (oper),Y
            .byte $29,$06,$a2   ; AND #oper
            .byte $25,$06,$72   ; AND oper
            .byte $35,$06,$82   ; AND oper,X
            .byte $2d,$06,$42   ; AND oper
            .byte $3d,$06,$52   ; AND oper,X
            .byte $39,$06,$62   ; AND oper,Y
            .byte $21,$06,$22   ; AND (oper,X)
            .byte $31,$06,$32   ; AND (oper),Y
            .byte $0a,$00,$b6   ; ASL A
            .byte $06,$00,$76   ; ASL oper
            .byte $16,$00,$86   ; ASL oper,X
            .byte $0e,$00,$46   ; ASL oper
            .byte $1e,$00,$56   ; ASL oper,X
            .byte $24,$25,$7b   ; BIT oper
            .byte $2c,$25,$4b   ; BIT oper
            .byte $30,$10,$c4   ; BMI oper
            .byte $10,$0c,$c6   ; BPL oper
            .byte $00,$2c,$b5   ; BRK
            .byte $18,$1a,$b1   ; CLC
            .byte $38,$01,$b1   ; SEC
            .byte $78,$01,$b4   ; SEI
            .byte $58,$1a,$b4   ; CLI
            .byte $c6,$0a,$71   ; DEC oper
            .byte $d6,$0a,$81   ; DEC oper,X
            .byte $ce,$0a,$41   ; DEC oper
            .byte $de,$0a,$51   ; DEC oper,X
            .byte $e6,$26,$71   ; INC oper
            .byte $f6,$26,$81   ; INC oper,X
            .byte $ee,$26,$41   ; INC oper
            .byte $fe,$26,$51   ; INC oper,X
            .byte $4a,$0e,$b9   ; LSR A
            .byte $46,$0e,$79   ; LSR oper
            .byte $56,$0e,$89   ; LSR oper,X
            .byte $4e,$0e,$49   ; LSR oper
            .byte $5e,$0e,$59   ; LSR oper,X
            .byte $48,$17,$b0   ; PHA
            .byte $08,$17,$b7   ; PHP
            .byte $68,$0d,$b0   ; PLA
            .byte $28,$0d,$b7   ; PLP
            .byte $2a,$2d,$b6   ; ROL A
            .byte $26,$2d,$76   ; ROL oper
            .byte $36,$2d,$86   ; ROL oper,X
            .byte $2e,$2d,$46   ; ROL oper
            .byte $3e,$2d,$56   ; ROL oper,X
            .byte $6a,$2d,$b9   ; ROR A
            .byte $66,$2d,$79   ; ROR oper
            .byte $76,$2d,$89   ; ROR oper,X
            .byte $6e,$2d,$49   ; ROR oper
            .byte $7e,$2d,$59   ; ROR oper,X
            .byte $09,$03,$a0   ; ORA #oper
            .byte $05,$03,$70   ; ORA oper
            .byte $15,$03,$80   ; ORA oper,X
            .byte $0d,$03,$40   ; ORA oper
            .byte $1d,$03,$50   ; ORA oper,X
            .byte $19,$03,$60   ; ORA oper,Y
            .byte $01,$03,$20   ; ORA (oper,X)
            .byte $11,$03,$30   ; ORA (oper),Y
            .byte $49,$02,$a9   ; EOR #oper
            .byte $45,$02,$79   ; EOR oper
            .byte $55,$02,$89   ; EOR oper,X
            .byte $4d,$02,$49   ; EOR oper
            .byte $5d,$02,$59   ; EOR oper,X
            .byte $59,$02,$69   ; EOR oper,Y
            .byte $41,$02,$29   ; EOR (oper,X)
            .byte $51,$02,$39   ; EOR (oper),Y
            .byte $40,$04,$b4   ; RTI
            .byte $e9,$0f,$a1   ; SBC #oper
            .byte $e5,$0f,$71   ; SBC oper
            .byte $f5,$0f,$81   ; SBC oper,X
            .byte $ed,$0f,$41   ; SBC oper
            .byte $fd,$0f,$51   ; SBC oper,X
            .byte $f9,$0f,$61   ; SBC oper,Y
            .byte $e1,$0f,$21   ; SBC (oper,X)
            .byte $f1,$0f,$31   ; SBC (oper),Y
            .byte $f8,$01,$b2   ; SED
            .byte $d8,$1a,$b2   ; CLD
            .byte $b8,$1a,$bc   ; CLV
            .byte $50,$2a,$c1   ; BVC oper
            .byte $70,$2a,$ca   ; BVS oper
            .byte TABLE_END     ; End of 6502 table
