;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                                      wAx
;                            Integrated Monitor Tools
;                             (c)2020, Jason Justian
;                  
; Release 1 - May 16, 2020
; Release 2 - May 23, 2020
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
TCHAR       = $b2               ; Wedge character = for tester
BCHAR       = "!"               ; Wedge character ! for breakpoint
RCHAR       = ";"               ; Wedge character ; for register set
ECHAR       = $5f               ; Wedge character arrow for code execute

; System resources
IGONE       = $0308             ; Vector to GONE
CBINV       = $0316             ; BRK vector
GONE        = $c7e4
CHRGET      = $0073
CHRGOT      = $0079
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
ACC         = $030c             ; Saved Accumulator
XREG        = $030d             ; Saved X Register
YREG        = $030e             ; Saved Y Register
PROC        = $030f             ; Saved Processor Status
ERROR_PTR   = $22               ; BASIC error text pointer
SYS_DEST    = $14               ; Pointer for SYS destination
MISMATCH    = $c2cd             ; "MISMATCH"

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
MNEM        = $a3               ; Current Mnemonic (2 bytes)
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
OUTBUFFER   = $0218             ; Output buffer (24 bytes)
INBUFFER    = $0230             ; Input buffer (22 bytes)
ZP_TMP      = $0246             ; Zeropage Preservation (16 bytes)
BREAKPOINT  = $0256             ; Breakpoint data (3 bytes)

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
            cmp #TCHAR          ; Tester
            beq Disp_Test       ; ,,
            cmp #BCHAR          ; Breakpoint Manager
            beq Disp_BP         ; ,,
            cmp #RCHAR          ; Register Setter
            beq Disp_Reg        ; ,,
            cmp #ECHAR          ; Execute
            beq Disp_Exec       ; ,,
            jsr CHRGOT          ; Restore flags for the found character
            jmp GONE+3          ; +3 because the CHRGET is already done
                        
; Dispatch Disassembler  
; https://github.com/Chysn/wAx/wiki/1-6502-Disassembler          
Disp_Dasm:  jsr Prepare
            jsr DisList
            jmp Return    

; Dispatch Assembler
; https://github.com/Chysn/wAx/wiki/2-6502-Assembler
Disp_Asm:   jsr Prepare
            jsr Assemble
            jmp Return
            
; Dispatch Memory Dump 
; https://github.com/Chysn/wAx/wiki/3-Memory-Dump           
Disp_Mem:   jsr Prepare
            jsr Memory
            jmp Return  

; Dispatch Hex Editor 
; https://github.com/Chysn/wAx/wiki/4-Hex-Editor           
Disp_Hex:   jsr Prepare
            jsr HexEditor
            jmp Return 
  
; Dispatch Register Editor           
; https://github.com/Chysn/wAx/wiki/5-Register-Editor
Disp_Reg:   jsr Prepare
            jsr Register
            jmp Return     

; Dispatch Subroutine Execution   
; https://github.com/Chysn/wAx/wiki/6-Subroutine-Execution    
Disp_Exec:  jsr Prepare
            jmp Execute
            
; Dispatch Assertion Tester           
; https://github.com/Chysn/wAx/wiki/8-Assertion-Tester
Disp_Test:  jsr Prepare
            jsr Tester
            bcs Return
            jmp Error
                                          
; Dispatch Breakpoint Manager
; https://github.com/Chysn/wAx/wiki/7-Breakpoint-Manager
Disp_BP:    jsr Prepare
            jsr BPManager
            ; Falls through to Return
    
; Return from Wedge
; Return in one of two ways:
; * In direct mode, to a BASIC warm start without READY.
; * In a program, find the next BASIC command
Return:     jsr Restore
            jsr DirectMode      ; If in Direct Mode, warm start without READY.
            bne in_program      ;   ,,
            jmp (WARM_START)    ;   ,,           
in_program: jmp NX_BASIC        ; Otherwise, continue to next BASIC command   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; DISASSEMBLER COMPONENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Disassembly Listing
; Disassemble multiple instructions, starting from the program counter
DisList:    jsr DirectMode      ; If the command is run in Direct Mode,
            bne d_listing       ;   cursor up to overwrite the original input
            lda #$91            ;   ,,
            jsr CHROUT          ;   ,,
d_listing:  ldx #DISPLAYL       ; Show this many lines of code
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

Unknown:    jsr HexPrefix       ;
            lda INSTDATA        ; The unknown opcode is still here   
            jsr Hex             ;
            lda #"?"            ;
            jmp disasm_r
            
; Write Mnemonic and Parameters
DMnemonic:  lda MNEM+1          ; Strip off the low bit of the low byte, which
            pha
            and #$fe            ;   indicates that the record is a mnemonic
            sta MNEM+1          ;   (encoding is big-endian)
            lda MNEM
            pha
            ldx #$03            ; Three characters...
-loop:      lda #$00
            sta CHARAC
            ldy #$05            ; Each character encoded in five bits, shifted
-shift_l:   lda #$00            ;   as a 24-bit register into CHARAC, which
            asl MNEM+1          ;   winds up as a ROT0 code (A=1 ... Z=26)
            rol MNEM            ;   ,,
            rol CHARAC          ;   ,,
            dey
            bne shift_l
            lda CHARAC
            adc #"@"            ; Get the PETSCII character. Carry is clear from
            jsr CharOut         ;   the last ROL
            dex
            bne loop
            pla
            sta MNEM
            pla
            sta MNEM+1
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
            bcc Error           ; Clear carry means the test failed
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
; Invalid opcode or formatting, or test failure
Error:      ldy FUNCTION        ; Stash which function is active
            jsr Restore         ; Restore zeropage state
            lda #<AsmErr        ; Default to ASSMEBLY Error
            ldx #>AsmErr        ; ,,
            cpy #TCHAR          ; If the function is Assertion Test,
            bne show_err        ;   then change the error to
            lda #<MISMATCH      ;   MISMATCH Error
            ldx #>MISMATCH      ;   ,,
show_err:   sta ERROR_PTR       ; Set the selected pointer
            stx ERROR_PTR+1     ;   ,,
            jmp BASICERR        ; And emit the error

; Get Operand
; Populate the operand for an instruction by looking forward in the buffer and
; counting upcoming hex digits.
GetOperand: jsr Buff2Byte       ; Get the first byte
            bcc getop_r         ; If invalid, return
            sta OPERAND+1       ; Default to being high byte
            jsr Buff2Byte
            bcs high_byte       ; If an 8-bit operand is provided, move the high
            lda OPERAND+1       ;   byte to the low byte. Otherwise, just
high_byte:  sta OPERAND         ;   set the low byte with the input
            sec                 ; Compute hypothetical relative branch
            sbc PRGCTR          ; Subtract the program counter address from
            sec                 ;   the instruction target
            sbc #$02            ; Offset by 2 to account for the instruction
            sta RB_OPERAND      ; Save the hypothetical relative branch operand
getop_r:    rts
            
; Hypothesis Test
; Search through the language table for each opcode and disassemble it using
; the opcode provided for the candidate instruction. If there's a match, then
; that's the instruction to assemble at the program counter. If Hypotest tries
; all the opcodes without a match, then the candidate instruction is invalid.
Hypotest:   lda PRGCTR+1        ; Save the program counter from the assembler
            pha                 ;   so it can be used by the disassembler
            lda PRGCTR
            pha
            jsr ResetLang       ; Reset language table
reset:      lda #OPCODE         ; Write location to PC for hypotesting
            sta PRGCTR          ; ,,
            ldy #$00            ; Set the program counter high byte
            sty PRGCTR+1        ; ,,
            sty IDX_OUT         ; Reset the output buffer for disassembly
            jsr NextInst        ; Get next instruction in 6502 table
            ldy #$00            ; Get the instruction's opcode
            lda (LANG_PTR),y    ;   ,,
            cmp #TABLE_END      ; If we've reached the end of the table,
            beq bad_code        ;   the assembly candidate is no good
            sta OPCODE          ; Store opcode to hypotesting location
            iny                 ; 
            lda (LANG_PTR),y    ; This is the addressing mode
            sta INSTDATA+1      ; Save addressing mode for disassembly
            jsr DMnemonic       ; Add mnemonic to buffer
            lda INSTDATA+1      ; If this is a relative branch instruction
            cmp #RELATIVE       ;   test it separately
            beq test_rel        ;   ,,
            jsr DOperand        ; Add formatted operand to buffer
            lda #$00            ; Add a $00 to the end of the disassembly as a
            jsr CharOut         ;   delimiter
            ldy #$00
-loop:      lda INBUFFER+4,y    ; Compare the assembly with the disassembly
            cmp OUTBUFFER,y     ;   in the buffers
            bne reset           ; If any bytes don't match, then skip
            iny
            cmp #$00
            bne loop            ; Loop until the buffer is done
match:      jsr NextValue
            lda PRGCTR          ; Set the CHRCOUNT location to the number of
            sec                 ;   bytes that need to be programmed
            sbc #OPCODE         ;   ,,
            sta CHRCOUNT        ;   ,,
            pla                 ; Restore the program counter so that the
            sta PRGCTR          ;   instruction is loaded to the right place
            pla                 ;   ,,
            sta PRGCTR+1        ;   ,,
            sec                 ; Set Carry flag to indicate success
            rts
test_rel:   ldy #$03            ; Here, relative branching instructions are
-loop:      lda OUTBUFFER,y     ;   handled. Only the first four characters
            cmp INBUFFER+4,y    ;   are compared. If there's a match on the
            bne reset           ;   mnemonic + $, then move the computed
            dey                 ;   relative operand into the regular operand
            bpl loop            ;   low byte, and then treat this as a regular
            lda RB_OPERAND      ;   match after that
            sta OPERAND         ;   ,,
            jsr NextValue
            jmp match           ; Treat this like a regular match from here
bad_code:   pla                 ; Pull the program counter off the stack, but
            pla                 ;   there's no need to do anything with it
            clc                 ;   because we're giving up.
            rts
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; MEMORY DUMP COMPONENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Memory:     jsr DirectMode      ; If the command is run in Direct Mode,
            bne m_listing       ;   cursor up to hide the original input
            lda #$91            ;   ,, 
            jsr CHROUT          ;   ,,
m_listing:  ldx #DISPLAYL       ; Show this many groups of four
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
; TEST COMPONENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Tester:     ldy #$00
-loop:      jsr Buff2Byte
            bcc test_r          ; Bail out on the first non-hex byte
            cmp (PRGCTR),y
            bne test_err      
            iny
            cpy #$04
            bne loop
test_r:     sec
            rts
test_err:   clc
            rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; BREAKPOINT COMPONENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
BPManager:  php
            jsr ClearBP         ; Clear the old breakpoint, if it exists
            plp                 ; If no breakpoint is chosen (e.g., if ! was)
            bcc bpm_r           ;   by itself), just clear the breakpoint
            lda PRGCTR          ; Add a new breakpoint at the program counter
            sta BREAKPOINT      ; ,,
            lda PRGCTR+1        ; ,,
            sta BREAKPOINT+1    ; ,,
            ldy #$00            ; Get the previous code
            lda (PRGCTR),y      ; Stash it in the Breakpoint data structure,
            sta BREAKPOINT+2    ;   to be restored on the next break
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

; BRK Trapper
; Replaces the default BRK handler. Shows the register display, goes to warm
; start.
Break:      cld                 ; Escape hatch for accidentally-set Decimal flag
            lda #$00
            sta IDX_OUT
            lda #<Registers     ; Print register display bar
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
            lda #$0d            ; Drop to the next line
            jsr CHROUT          ; ,,
            jmp (WARM_START)    
            
; Clear Breakpoint   
; Restore breakpoint byte and zero out breakpoint data         
ClearBP:    lda BREAKPOINT      ; Get the breakpoint
            sta CHARAC          ; Stash it in a zeropage location
            lda BREAKPOINT+1    ; ,,
            sta CHARAC+1        ; ,,
            ldy #$00
            lda (CHARAC),y      ; What's currently at the Breakpoint?
            bne bp_reset        ; If it's not a BRK, then preserve what's there
            lda BREAKPOINT+2    ; Otherwise, get the breakpoint byte and
            sta (CHARAC),y      ;   put it back 
bp_reset:   sty BREAKPOINT      ; And then clear out the whole
            sty BREAKPOINT+1    ;   breakpoint data structure
            sty BREAKPOINT+2    ;   ,,
cleared:    rts

; Breakpoint Indicator
; Also restores the breakpoint byte, temporarily
BreakInd:   ldy #$00            ; Is this a BRK instruction?
            lda (PRGCTR),y      ; ,,
            bne ind_r           ; If not, do nothing
            lda BREAKPOINT      ; If it is a BRK, is it our breakpoint?
            cmp PRGCTR          ; ,,
            bne ind_r           ; ,,
            lda BREAKPOINT+1    ; ,,
            cmp PRGCTR+1        ; ,,
            bne ind_r           ; ,,
            lda #$12            ; Reverse on for the breakpoint
            jsr CharOut
            lda BREAKPOINT+2    ; Temporarily restore the breakpoint byte
            sta (PRGCTR),y      ;   for disassembly purposes
ind_r:      rts        
                 
; Enable Breakpoint
; Used after disassembly, in case the BreakInd turned the breakpoint off
EnableBP:   lda BREAKPOINT+2
            beq enable_r
            lda BREAKPOINT
            sta CHARAC
            lda BREAKPOINT+1
            sta CHARAC+1
            ldy #$00            ; Write BRK to the breakpoint
            lda #$00            ; ,,
            sta (CHARAC),y      ; ,,
enable_r:   rts
             
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; REGISTER COMPONENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Register:   lda PRGCTR+1        ; Two bytes are already set in the program
            sta YREG            ;   counter. These are Y and X
            lda PRGCTR          ;   ,,
            sta XREG            ;   ,,
            jsr Buff2Byte       ; Get a third byte to set Accumulator
            sta ACC             ;   ,,
            jsr Buff2Byte       ; Get a fourth byte to set Processor Status
            sta PROC            ;   ,,
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
            sta ZP_TMP,x        ;   routine is done, the data will be restored
            inx                 ;   in Return
            cpx #$10            ;   ,,
            bne loop            ;   ,,
            sty FUNCTION        ; Store the mode (to normalize spaces in buffer)
            lda #$00            ; Initialize the input index for write
            sta IDX_IN          ; ,,
            jsr Transcribe      ; Transcribe from CHRGET to INBUFFER
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
-loop:      lda ZP_TMP,x        ;   ,,
            sta WORK,x          ;   ,,
            inx                 ;   ,,
            cpx #$10            ;   ,,
            bne loop            ;   ,,
            rts       

; Look up opcode
Lookup:     sta INSTDATA
            jsr ResetLang
-loop:      jsr NextInst
            ldy #$00
            lda (LANG_PTR),y
            cmp #TABLE_END
            beq not_found
            cmp INSTDATA
            bne loop
found:      iny
            lda (LANG_PTR),y    ; A match was found! Set the addressing mode
            sta INSTDATA+1      ;   to the instruction data structure
            sec                 ;   and set the carry flag to indicate success
            rts
not_found:  clc                 ; Reached the end of the language table without
            rts                 ;   finding a matching instruction
                                    
; Reset Language Table            
ResetLang:  lda #<InstrSet-2    ; Start two bytes before the Instruction Set
            sta LANG_PTR        ;   table, because advancing the table will be
            lda #>InstrSet-2    ;   an early thing we do
            sta LANG_PTR+1      ;   ,,
            rts
            
; Next Instruction in Language Table
; Handle mnemonics by recording the last found mnemonic and then advancing
; to the following instruction.
NextInst:   lda #$02            ; Each language entry is two bytes
            clc
            adc LANG_PTR
            sta LANG_PTR
            bcc ch_mnem
            inc LANG_PTR+1
ch_mnem:    ldy #$01            ; Is this entry an instruction record?
            lda (LANG_PTR),y    ; ,,
            and #$01            ; ,,
            beq adv_lang_r      ; If it's an instruction, return
            lda (LANG_PTR),y    ; Otherwise, set the mnemonic in the workspace
            sta MNEM+1
            dey
            lda (LANG_PTR),y
            sta MNEM
            jmp NextInst        ; Go to what should now be an instruction
adv_lang_r: rts
            
; Get Character
; Akin to CHRGET, but scans the INBUFFER, which has already been detokenized            
CharGet:    ldx IDX_IN
            lda INBUFFER,x
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
write_ok:   tya                 ; Save registers
            pha                 ; ,,
            txa                 ; ,,
            pha                 ; ,,
            ldx IDX_OUT         ; Write to the next OUTBUFFER location
            lda CHARAC          ; ,,
            sta OUTBUFFER,x     ; ,,
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
            sta INBUFFER,x
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
            lda #<OUTBUFFER     ; Print the line
            ldy #>OUTBUFFER     ; ,,
            jsr PRTSTR          ; ,,
            rts 
            
; Prompt for Next Line
; X should be set to the number of bytes the program counter should be
; advanced
Prompt:     jsr DirectMode      ; If a command is in Direct Mode, increase
            bne prompt_r        ;   the PC by the size of the instruction
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
-loop:      lda OUTBUFFER,y     ; Copy the output buffer into KEYBUFF, which
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
            
; In Direct Mode
; If the wAx command is running in Direct Mode, the Zero flag will be set
DirectMode: ldy CURLIN+1
            iny
            rts     
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Token table is used to detokenize certain BASIC keywords that might
; appear in assembly or addresses
Token:      .byte $96,$44,$45,$46   ; DEF
            .byte $af,$41,$4e,$44   ; AND
            .byte $b0,$4f,$52,$00   ; OR

; Miscellaneous data tables
HexDigit:   .asc "0123456789ABCDEF"
Intro:      .asc $0d,"WAX ON",$00
Registers:  .asc $0d,"BRK",$0d," Y: X: A: P: S: PC::",$0d,";",$00
AsmErr:     .asc "ASSEMBL",$d9

; Padding to 2048 bytes, done for two reasons
; (1) ROM images for 2716s
; (2) So that extended instructions sets can always be loaded into the
;     same place (base + $7ff)
Padding:    .asc "1234567890123456789012345678901234567890"

; Instruction Set
; This table contains two types of one-word records--mnemonic records and
; instruction records. Every word in the table is in big-endian format, so
; the high byte is first.
;
; Mnemonic records are formatted like this...
;     fffffsss ssttttt1
; where f is first letter, s is second letter, and t is third letter. Bit
; 0 of the word is set to 1 to identify this word as a mnemonic record.
;
; Each mnemonic record has one or more instruction records after it.
; Instruction records are formatted like this...
;     oooooooo aaaaaaa0
; where o is the opcode and a is the addressing mode (see Constants section
; at the top of the code). Bit 0 of the word is set to 0 to identify this
; word as an instruction record.
InstrSet:   .byte $09,$07       ; ADC
            .byte $69,$a0       ; * ADC #immediate
            .byte $65,$70       ; * ADC zeropage
            .byte $75,$80       ; * ADC zeropage,X
            .byte $6d,$40       ; * ADC absolute
            .byte $7d,$50       ; * ADC absolute,X
            .byte $79,$60       ; * ADC absolute,Y
            .byte $61,$20       ; * ADC (indirect,X)
            .byte $71,$30       ; * ADC (indirect),Y
            .byte $0b,$89       ; AND
            .byte $29,$a0       ; * AND #immediate
            .byte $25,$70       ; * AND zeropage
            .byte $35,$80       ; * AND zeropage,X
            .byte $2d,$40       ; * AND absolute
            .byte $3d,$50       ; * AND absolute,X
            .byte $39,$60       ; * AND absolute,Y
            .byte $21,$20       ; * AND (indirect,X)
            .byte $31,$30       ; * AND (indirect),Y
            .byte $0c,$d9       ; ASL
            .byte $0a,$a0       ; * ASL accumulator
            .byte $06,$70       ; * ASL zeropage
            .byte $16,$80       ; * ASL zeropage,X
            .byte $0e,$40       ; * ASL absolute
            .byte $1e,$50       ; * ASL absolute,X
            .byte $10,$c7       ; BCC
            .byte $90,$c0       ; * BCC relative
            .byte $10,$e7       ; BCS
            .byte $b0,$c0       ; * BCS relative
            .byte $11,$63       ; BEQ
            .byte $f0,$c0       ; * BEQ relative
            .byte $12,$69       ; BIT
            .byte $24,$70       ; * BIT zeropage
            .byte $2c,$40       ; * BIT absolute
            .byte $13,$53       ; BMI
            .byte $30,$c0       ; * BMI relative
            .byte $13,$8b       ; BNE
            .byte $d0,$c0       ; * BNE relative
            .byte $14,$19       ; BPL
            .byte $10,$c0       ; * BPL relative
            .byte $14,$97       ; BRK
            .byte $00,$b0       ; * BRK implied
            .byte $15,$87       ; BVC
            .byte $50,$c0       ; * BVC relative
            .byte $15,$a7       ; BVS
            .byte $70,$c0       ; * BVS relative
            .byte $1b,$07       ; CLC
            .byte $18,$b0       ; * CLC implied
            .byte $1b,$09       ; CLD
            .byte $d8,$b0       ; * CLD implied
            .byte $1b,$13       ; CLI
            .byte $58,$b0       ; * CLI implied
            .byte $1b,$2d       ; CLV
            .byte $b8,$b0       ; * CLV implied
            .byte $1b,$61       ; CMP
            .byte $c9,$a0       ; * CMP #immediate
            .byte $c5,$70       ; * CMP zeropage
            .byte $d5,$80       ; * CMP zeropage,X
            .byte $cd,$40       ; * CMP absolute
            .byte $dd,$50       ; * CMP absolute,X
            .byte $d9,$60       ; * CMP absolute,Y
            .byte $c1,$20       ; * CMP (indirect,X)
            .byte $d1,$30       ; * CMP (indirect),Y
            .byte $1c,$31       ; CPX
            .byte $e0,$a0       ; * CPX #immediate
            .byte $e4,$70       ; * CPX zeropage
            .byte $ec,$40       ; * CPX absolute
            .byte $1c,$33       ; CPY
            .byte $c0,$a0       ; * CPY #immediate
            .byte $c4,$70       ; * CPY zeropage
            .byte $cc,$40       ; * CPY absolute
            .byte $21,$47       ; DEC
            .byte $c6,$70       ; * DEC zeropage
            .byte $d6,$80       ; * DEC zeropage,X
            .byte $ce,$40       ; * DEC absolute
            .byte $de,$50       ; * DEC absolute,X
            .byte $21,$71       ; DEX
            .byte $ca,$b0       ; * DEX implied
            .byte $21,$73       ; DEY
            .byte $88,$b0       ; * DEY implied
            .byte $2b,$e5       ; EOR
            .byte $49,$a0       ; * EOR #immediate
            .byte $45,$70       ; * EOR zeropage
            .byte $55,$80       ; * EOR zeropage,X
            .byte $4d,$40       ; * EOR absolute
            .byte $5d,$50       ; * EOR absolute,X
            .byte $59,$60       ; * EOR absolute,Y
            .byte $41,$20       ; * EOR (indirect,X)
            .byte $51,$30       ; * EOR (indirect),Y
            .byte $4b,$87       ; INC
            .byte $e6,$70       ; * INC zeropage
            .byte $f6,$80       ; * INC zeropage,X
            .byte $ee,$40       ; * INC absolute
            .byte $fe,$50       ; * INC absolute,X
            .byte $4b,$b1       ; INX
            .byte $e8,$b0       ; * INX implied
            .byte $4b,$b3       ; INY
            .byte $c8,$b0       ; * INY implied
            .byte $53,$61       ; JMP
            .byte $4c,$40       ; * JMP absolute
            .byte $6c,$10       ; * JMP indirect
            .byte $54,$e5       ; JSR
            .byte $20,$40       ; * JSR absolute
            .byte $61,$03       ; LDA
            .byte $a9,$a0       ; * LDA #immediate
            .byte $a5,$70       ; * LDA zeropage
            .byte $b5,$80       ; * LDA zeropage,X
            .byte $ad,$40       ; * LDA absolute
            .byte $bd,$50       ; * LDA absolute,X
            .byte $b9,$60       ; * LDA absolute,Y
            .byte $a1,$20       ; * LDA (indirect,X)
            .byte $b1,$30       ; * LDA (indirect),Y
            .byte $61,$31       ; LDX
            .byte $a2,$a0       ; * LDX #immediate
            .byte $a6,$70       ; * LDX zeropage
            .byte $b6,$90       ; * LDX zeropage,Y
            .byte $ae,$40       ; * LDX absolute
            .byte $be,$60       ; * LDX absolute,Y
            .byte $61,$33       ; LDY
            .byte $a0,$a0       ; * LDY #immediate
            .byte $a4,$70       ; * LDY zeropage
            .byte $b4,$80       ; * LDY zeropage,X
            .byte $ac,$40       ; * LDY absolute
            .byte $bc,$50       ; * LDY absolute,X
            .byte $64,$e5       ; LSR
            .byte $4a,$a0       ; * LSR accumulator
            .byte $46,$70       ; * LSR zeropage
            .byte $56,$80       ; * LSR zeropage,X
            .byte $4e,$40       ; * LSR absolute
            .byte $5e,$50       ; * LSR absolute,X
            .byte $73,$e1       ; NOP
            .byte $ea,$b0       ; * NOP implied
            .byte $7c,$83       ; ORA
            .byte $09,$a0       ; * ORA #immediate
            .byte $05,$70       ; * ORA zeropage
            .byte $15,$80       ; * ORA zeropage,X
            .byte $0d,$40       ; * ORA absolute
            .byte $1d,$50       ; * ORA absolute,X
            .byte $19,$60       ; * ORA absolute,Y
            .byte $01,$20       ; * ORA (indirect,X)
            .byte $11,$30       ; * ORA (indirect),Y
            .byte $82,$03       ; PHA
            .byte $48,$b0       ; * PHA implied
            .byte $82,$21       ; PHP
            .byte $08,$b0       ; * PHP implied
            .byte $83,$03       ; PLA
            .byte $68,$b0       ; * PLA implied
            .byte $83,$21       ; PLP
            .byte $28,$b0       ; * PLP implied
            .byte $93,$d9       ; ROL
            .byte $2a,$a0       ; * ROL accumulator
            .byte $26,$70       ; * ROL zeropage
            .byte $36,$80       ; * ROL zeropage,X
            .byte $2e,$40       ; * ROL absolute
            .byte $3e,$50       ; * ROL absolute,X
            .byte $93,$e5       ; ROR
            .byte $6a,$a0       ; * ROR accumulator
            .byte $66,$70       ; * ROR zeropage
            .byte $76,$80       ; * ROR zeropage,X
            .byte $6e,$40       ; * ROR absolute
            .byte $7e,$50       ; * ROR absolute,X
            .byte $95,$13       ; RTI
            .byte $40,$b0       ; * RTI implied
            .byte $95,$27       ; RTS
            .byte $60,$b0       ; * RTS implied
            .byte $98,$87       ; SBC
            .byte $e9,$a0       ; * SBC #immediate
            .byte $e5,$70       ; * SBC zeropage
            .byte $f5,$80       ; * SBC zeropage,X
            .byte $ed,$40       ; * SBC absolute
            .byte $fd,$50       ; * SBC absolute,X
            .byte $f9,$60       ; * SBC absolute,Y
            .byte $e1,$20       ; * SBC (indirect,X)
            .byte $f1,$30       ; * SBC (indirect),Y
            .byte $99,$47       ; SEC
            .byte $38,$b0       ; * SEC implied
            .byte $99,$49       ; SED
            .byte $f8,$b0       ; * SED implied
            .byte $99,$53       ; SEI
            .byte $78,$b0       ; * SEI implied
            .byte $9d,$03       ; STA
            .byte $85,$70       ; * STA zeropage
            .byte $95,$80       ; * STA zeropage,X
            .byte $8d,$40       ; * STA absolute
            .byte $9d,$50       ; * STA absolute,X
            .byte $99,$60       ; * STA absolute,Y
            .byte $81,$20       ; * STA (indirect,X)
            .byte $91,$30       ; * STA (indirect),Y
            .byte $9d,$31       ; STX
            .byte $86,$70       ; * STX zeropage
            .byte $96,$90       ; * STX zeropage,Y
            .byte $8e,$40       ; * STX absolute
            .byte $9d,$33       ; STY
            .byte $84,$70       ; * STY zeropage
            .byte $94,$80       ; * STY zeropage,X
            .byte $8c,$40       ; * STY absolute
            .byte $a0,$71       ; TAX
            .byte $aa,$b0       ; * TAX implied
            .byte $a0,$73       ; TAY
            .byte $a8,$b0       ; * TAY implied
            .byte $a4,$f1       ; TSX
            .byte $ba,$b0       ; * TSX implied
            .byte $a6,$03       ; TXA
            .byte $8a,$b0       ; * TXA implied
            .byte $a6,$27       ; TXS
            .byte $9a,$b0       ; * TXS implied
            .byte $a6,$43       ; TYA
            .byte $98,$b0       ; * TYA implied
Expand:     .byte TABLE_END     ; End of 6502 table
