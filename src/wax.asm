;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                                      wAx
;                            Integrated Monitor Tools
;                             (c)2020, Jason Justian
;                  
; Release 1 - May 16, 2020
; Release 2 - May 23, 2020
; Release 3 - May 30, 2020
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
LIST_NUM    = $10               ; Display this many lines
TOOL_COUNT  = $0a               ; How many tools are there?
T_DIS       = "."               ; Wedge character . for disassembly
T_ASM       = "@"               ; Wedge character @ for assembly
T_MEM       = ","               ; Wedge character , for memory dump
T_BIN       = "'"               ; Wedge character ' for binary dump
T_TST       = $b2               ; Wedge character = for tester
T_BRK       = "!"               ; Wedge character ! for breakpoint
T_REG       = ";"               ; Wedge character ; for register set
T_EXE       = $5f               ; Wedge character left-arrow for code execute
T_SAV       = $b1               ; Wedge character > for save
T_LOA       = $b3               ; Wedge character < for load
BYTE        = ":"               ; .byte entry character
FWDR        = "*"               ; Forward relative branch character
BINARY      = "%"               ; Binary entry character
DEVICE      = $08               ; Save device

; System resources - Routines
GONE        = $c7e4
CHRGET      = $0073
CHRGOT      = $0079
PRTSTR      = $cb1e             ; Print from data (Y,A)
PRTFIX      = $ddcd             ; Print base-10 number
SYS         = $e133             ; BASIC SYS start
CHROUT      = $ffd2
WARM_START  = $0302             ; BASIC warm start vector
READY       = $c002             ; BASIC warm start with READY.
NX_BASIC    = $c7ae             ; Get next BASIC command
CUST_ERR    = $c447             ; Custom BASIC error message
SYNTAX_ERR  = $cf08             ; BASIC syntax error
ERROR_NO    = $c43b             ; Show error in Accumulator
SETLFS      = $ffba             ; Setup logical file
SETNAM      = $ffbd             ; Setup file name
SAVE        = $ffd8             ; Save
LOAD        = $ffd5             ; Load
CLOSE       = $ffc3             ; Close logical file

; System resources - Vectors and Pointers
IGONE       = $0308             ; Vector to GONE
CBINV       = $0316             ; BRK vector
BUFPTR      = $7a               ; Pointer to buffer
ERROR_PTR   = $22               ; BASIC error text pointer
SYS_DEST    = $14               ; Pointer for SYS destination

; System resources - Data
KEYWORDS    = $c09e             ; Start of BASIC kewords for detokenize
BUF         = $0200             ; Input buffer
CHARAC      = $07               ; Temporary character
KEYBUFF     = $0277             ; Keyboard buffer and size, for automatically
CURLIN      = $39               ; Current line number
KBSIZE      = $c6               ;   advancing the assembly address
MISMATCH    = $c2cd             ; "MISMATCH"
KEYCVTRS    = $028d             ; Keyboard codes
LSTX        = $c5               ; Keyboard matrix

; System resources - Registers
ACC         = $030c             ; Saved Accumulator
XREG        = $030d             ; Saved X Register
YREG        = $030e             ; Saved Y Register
PROC        = $030f             ; Saved Processor Status

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
LF          = $0d               ; Linefeed
CRSRUP      = $91               ; Cursor up
CRSRRT      = $1d               ; Cursor right
RVS_ON      = $12               ; Reverse on
RVS_OFF     = $92               ; Reverse off

; Assembler workspace
WORK        = $a3               ; Temporary workspace (2 bytes)
MNEM        = $a3               ; Current Mnemonic (2 bytes)
PRGCTR      = $a5               ; Program Counter (2 bytes)
CHARDISP    = $a7               ; Character display for Memory (2 bytes)
LANG_PTR    = $a7               ; Language Pointer (2 bytes)
INSTDATA    = $a9               ; Instruction data (2 bytes)
TOOL_CHR    = $ab               ; Current function (T_ASM, T_DIS)
OPCODE      = $ac               ; Assembly target for hypotesting
OPERAND     = $ad               ; Operand storage (2 bytes)
SP_OPERAND  = $af               ; Hypothetical relative branch operand
INSTSIZE    = $b0               ; Instruction size
IDX_IN      = $b1               ; Buffer index
IDX_OUT     = $b2               ; Buffer index
RB_FORWARD  = $10               ; Relative branch instruction address (2 bytes)
OUTBUFFER   = $0218             ; Output buffer (24 bytes)
INBUFFER    = $0230             ; Input buffer (22 bytes)
ZP_TMP      = $0246             ; Zeropage Preservation (16 bytes)
BREAKPOINT  = $0256             ; Breakpoint data (3 bytes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; INSTALLER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
Install:    jsr $c533           ; Re-chain BASIC program to set BASIC
            lda $22             ;   pointers as a courtesy to the user
            ;clc                ;   ,, ($c533 always exits with Carry clear)
            adc #$02            ;   ,,
            sta $2D             ;   ,,
            lda $23             ;   ,,
            jsr $C655           ;   ,,
            jsr SetupVec        ; Set up vectors (IGONE and BRK)
            lda #<Intro         ; Announce that wAx is on
            ldy #>Intro         ; ,,
            jsr PRTSTR          ; ,,
            jmp (READY)         ; READY.
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; MAIN PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;              
main:       jsr CHRGET          ; Get the character from input or BASIC
            ldy #$00            ; Is it one of the wedge characters?
-loop:      cmp ToolTable,y     ; ,,
            beq Prepare         ; If so, run the selected tool
            iny                 ; Else, check the characters in turn
            cpy #TOOL_COUNT     ; ,,
            bne loop            ; ,,
            jsr CHRGOT          ; Restore flags for the found character
            jmp GONE+3          ; +3 because the CHRGET is already done

; Prepare for Tool Run
; A wedge character has been entered, and will now be interpreted as a wedge
; command. Prepare for execution by
; (1) Setting a return point
; (2) Putting the tool's start address-1 on the stack
; (3) Saving the zeropage workspace for future restoration
; (4) Transcribing from BASIC or input buffer to the wAx input buffer
; (5) Reading the first four hexadecimal characters used by all wAx tools and
;     setting the Carry flag if there's a valid 16-bit number provided
; (6) RTS to route to the selected tool            
Prepare:    tax                 ; Save A in X so Prepare can set TOOL_CHR
            lda #>Return-1      ; Push the address-1 of Return onto the stack
            pha                 ;   as the destination for RTS of the
            lda #<Return-1      ;   selected tool
            pha                 ;   ,,
            lda ToolAddr_H,y    ; Push the looked-up address-1 of the selected
            pha                 ;   tool onto the stack. The RTS below will
            lda ToolAddr_L,y    ;   pull off the address and route execution
            pha                 ;   to the appropriate tool
            ldy #$00            ; wAx is to be zeropage-neutral, so preserve
-loop:      lda WORK,y          ;   its workspace in temp storage. When this
            sta ZP_TMP,y        ;   routine is done, the data will be restored
            iny                 ;   in Return
            cpy #$10            ;   ,,
            bne loop            ;   ,,
            stx TOOL_CHR        ; Store the tool character
            lda #$00            ; Initialize the input index for write
            sta IDX_IN          ; ,,
            jsr Transcribe      ; Transcribe from CHRGET to INBUFFER
            lda #$ef            ; $0082 BEQ $008a -> BEQ $0073 (maybe)
            sta $83             ; ,,
RefreshPC:  lda #$00            ; Re-initialize for buffer read
            sta IDX_IN          ; ,,
            jsr Buff2Byte       ; Convert 2 characters to a byte   
            bcc main_r          ; Fail if the byte couldn't be parsed
            sta PRGCTR+1        ; Save to the PRGCTR high byte
            jsr Buff2Byte       ; Convert next 2 characters to byte
            bcc main_r          ; Fail if the byte couldn't be parsed
            sta PRGCTR          ; Save to the PRGCTR low byte
main_r:     rts                 ; Pull address-1 off stack and go there
    
; Return from Wedge
; Return in one of two ways--
; (1) In direct mode, to a BASIC warm start without READY.
; (2) In a program, find the next BASIC command
Return:     jsr Restore
            jsr DirectMode      ; If in Direct Mode, warm start without READY.
            bne in_program      ;   ,,
            jmp (WARM_START)    ;   ,,           
in_program: jmp NX_BASIC        ; Otherwise, continue to next BASIC command   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; COMMON LIST COMPONENT
; Shared entry point for Disassembler and Memory Dump
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
List:       bcc list_r          ; Bail if the address is no good
            jsr DirectMode      ; If the tool is in direct mode,
            bne start_list      ;   cursor up to overwrite the original input
            lda #CRSRUP         ;   ,,
            jsr CHROUT          ;   ,,
start_list: ldx #LIST_NUM       ; Default if no number has been provided
ListLine:   txa
            pha
            lda #$00
            sta IDX_OUT
            jsr BreakInd        ; Indicate breakpoint, if it's here
            lda TOOL_CHR        ; Start each line with the wedge character, so
            jsr CharOut         ;   the user can chain commands
            lda PRGCTR+1        ; Show the address
            jsr Hex             ; ,,
            lda PRGCTR          ; ,,
            jsr Hex             ; ,,            
            lda TOOL_CHR        ; What tool is being used?
            cmp #T_MEM          ; Memory Dump
            beq to_mem          ; ,,
            cmp #T_BIN          ; Binary Dump
            beq to_bin          ; ,,
            jsr Space           ; Space goes after address for Disassembly
            jsr Disasm
            jmp continue
to_mem:     lda #BYTE           ; The .byte entry character goes after the
            jsr CharOut         ;   address for memory display
            jsr Memory          ;   ,,
            jmp continue
to_bin:     lda #BINARY         ; The binary entry character goes after the
            jsr CharOut         ;   address for binary display
            jsr BinaryDisp      ;   ,,          
continue:   jsr PrintBuff      
            pla
            tax
            ldy LSTX            ; Exit if STOP key is pressed
            cpy #$18            ; ,,          
            beq list_r          ; ,,
            dex                 ; Exit if loop is done
            bne ListLine        ; ,,
            inx                 ; But if the loop is done, but a SHift key
            lda KEYCVTRS        ;   is engaged, then go back for one more
            and #$01            ;   ,,
            bne ListLine        ;   ,,
list_r:     jmp EnableBP        ; Re-enable breakpoint, if necessary

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; DISASSEMBLER COMPONENTS
; https://github.com/Chysn/wAx/wiki/1-6502-Disassembler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Disassemble
; Disassemble a single instruction at the program counter
Disasm:     ldy #$00            ; Get the opcode
            lda (PRGCTR),y      ;   ,,
            jsr Lookup          ; Look it up
            bcc Unknown         ; Clear carry indicates an unknown opcode
            jsr DMnemonic       ; Display mnemonic
            jsr Space
            lda INSTDATA+1      ; Pass addressing mode to operand routine
            jsr DOperand        ; Display operand
            jmp NextValue       ; Advance to the next line of code

; Unknown Opcode
Unknown:    lda #BYTE           ; Byte entry before an unknown byte
            jsr CharOut         ; ,,
            lda INSTDATA        ; The unknown opcode is still here   
            jsr Hex             
            jmp NextValue
            
; Mnemonic Display
DMnemonic:  lda MNEM+1          ; These locations are going to rotated, so
            pha                 ;   save them on a stack for after the
            lda MNEM            ;   display
            pha                 ;   ,,
            ldx #$03            ; Three characters...
-loop:      lda #$00
            sta CHARAC
            ldy #$05            ; Each character encoded in five bits, shifted
shift_l:    lda #$00            ;   as a 24-bit register into CHARAC, which
            asl MNEM+1          ;   winds up as a ROT0 code (A=1 ... Z=26)
            rol MNEM            ;   ,,
            rol CHARAC          ;   ,,
            dey
            bne shift_l
            lda CHARAC
            ;clc                ; Carry is clear from the last ROL
            adc #"@"            ; Get the PETSCII character
            jsr CharOut
            dex
            bne loop
            pla
            sta MNEM
            pla
            sta MNEM+1
mnemonic_r: rts

; Operand Display
; Dispatch display routines based on addressing mode
DOperand:   cmp #IMPLIED        ; Handle each addressing mode with a subroutine
            beq mnemonic_r      ; Implied has no operand, so it goes to some RTS
            cmp #RELATIVE
            beq DisRel
            cmp #IMMEDIATE
            beq DisImm
            cmp #ZEROPAGE       ; Subsumes all zeropage modes
            bcs DisZP
            cmp #ABSOLUTE       ; Subsumes all absolute modes
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
            jmp CloseParen
ind_xy:     pha
            jsr Param_8
            pla
            cmp #INDIRECT_X
            bne ind_y
            jsr Comma
            lda #"X"
            jsr CharOut
            jmp CloseParen
ind_y:      jsr CloseParen
            jsr Comma
            lda #"Y"
            jmp CharOut

; Disassemble Immediate Operand         
DisImm:     lda #"#"
            jsr CharOut
            jmp Param_8

; Disassemble Zeropage Operand
DisZP:      pha
            jsr Param_8
            pla
            sec
            sbc #ZEROPAGE
            jmp draw_xy         ; From this point, it's the same as Absolute            

; Disassemble Relative Operand
DisRel:     jsr HexPrefix
            jsr NextValue       ; Get the operand of the instruction, advance
                                ;   the program counter. It might seem weird to
                                ;   advance the PC when I'm operating on it a
                                ;   few lines down, but I need to add two
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
            jmp Hex             ; ,,
                            
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
abs_ind:    jsr Comma           ; This is an indexed addressing mode, so
            txa                 ;   write a comma and index register
            jmp CharOut         ;   ,,
                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; MEMORY EDITOR COMPONENTS
; https://github.com/Chysn/wAx/wiki/4-Memory-Editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MemEditor:  ldy #$00            ; This is Assemble's entry point for .byte
-loop:      jsr Buff2Byte
            bcc edit_exit       ; Bail out on the first non-hex byte
            sta (PRGCTR),y      
            iny
            cpy #$04
            bne loop
edit_exit:  cpy #$00
            beq asm_error
            tya
            tax
            jsr Prompt          ; Prompt for the next address
            jsr ClearBP         ; Clear breakpoint if anything was changed
edit_r:     rts

; Text Editor
; If the input starts with a quote, add characters until we reach another
; quote, or 0
TextEdit:   ldy #$00            ; Y=Data Index
-loop:      jsr CharGet
            beq asm_error       ; Return to MemEditor if 0
            cmp #QUOTE          ; Is this the closing quote?
            beq edit_exit       ; Return to MemEditor if quote
            sta (PRGCTR),y      ; Populate data
            iny
            cpy #$10            ; String size limit
            beq edit_exit
            jmp loop
            
; Binary Editor
; If the input starts with a %, get one binary byte and store it in memory                   
BinaryEdit: jsr BinaryByte      ; Get 8 binary bits
            bcc edit_r          ; If invalid, exit assembler
            ldy #$00            ; Store the valid byte to memory
            sta (PRGCTR),y      ; ,,
            iny                 ; Increment the byte count and return to
            jmp edit_exit       ;   editor            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; ASSEMBLER COMPONENTS
; https://github.com/Chysn/wAx/wiki/2-6502-Assembler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Assemble:   bcc asm_r           ; Bail if the address is no good
            lda INBUFFER+4      ; If the user just pressed Return at the prompt,
            beq asm_r           ;   go back to BASIC
-loop:      jsr CharGet         ; Look through the buffer for either
            beq test            ;   0, which should indicate implied mode, or:
            cmp #FWDR           ; * = Handle forward relative branching
            beq HandleFwd       ; ,,
            cmp #BYTE           ; . = Start .byte entry (route to hex editor)
            beq MemEditor       ; ,,
            cmp #QUOTE          ; " = Start text entry (route to text editor)
            beq TextEdit        ; ,,
            cmp #BINARY         ; % = Start binary entry (route to binary editor)
            beq BinaryEdit      ; ,,
            cmp #"#"            ; # = Parse immediate operand (quotes and %)
            beq ImmedOp         ; ,,            
            cmp #"$"            ; $ = Parse the operand
            bne loop            ; ,,
            jsr GetOperand      ; Once $ is found, then grab the operand
test:       jsr Hypotest        ; Line is done; hypothesis test for a match
            bcc asm_error       ; Clear carry means the test failed
            ldy #$00            ; A match was found! Transcribe the good code
            lda OPCODE          ;   to the program counter. The number of bytes
            sta (PRGCTR),y      ;   to transcribe is stored in the INSTSIZE memory
            ldx INSTSIZE        ;   location.
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
asm_error:  jmp AsmError

; Handle Forward Branch
; In cases where the forward branch address is unknown, * may be used as
; the operand for a relative branch instruction. The branch may be resolved
; by entering * on a line by itself, after the address.
HandleFwd:  lda IDX_IN          ; Where in the line does the * appear?
            cmp #$05            ; If it's right after the address, it's for
            beq resolve_fw      ;   resolution of the forward branch point
set_fw:     lda PRGCTR          ; Otherwise, it's to set the forward branch
            sta RB_FORWARD      ;   point. Store the location of the branch
            lda PRGCTR+1        ;   instruction.
            sta RB_FORWARD+1    ;   ,,
            lda #$00            ; Aim the branch instruction at the next
            sta SP_OPERAND      ;   instruction by default
            ldy IDX_IN          ; Change the character in the buffer from
            lda #"$"            ;   * to $, so that the instruction can pass
            sta INBUFFER-1,y    ;   the hypotesting
            jmp test            ; Go back to assemble the instruction
resolve_fw: lda PRGCTR          ; Compute the relative branch offset from the
            sec                 ;   current program counter
            sbc RB_FORWARD      ;   ,,
            sec                 ; Offset by 2 to account for the instruction
            sbc #$02            ;   ,,
            ldy #$01            ; Save the computed offset right after the
            sta (RB_FORWARD),y  ;   original instruction as its operand
            ldx #$00            ; Prompt for the same memory location again
            jmp Prompt          ; ,,
 
; Parse Immediate Operand
; Immediate operand octets are expressed in the following formats--
; (1) $dd       - Hexadecimal 
; (2) "c"       - Character
; (3) %bbbbbbbb - Binary
ImmedOp:    jsr CharGet
            cmp #"$"
            bne try_quote
            jsr GetOperand
            lda OPERAND
            sta SP_OPERAND
            jmp test
try_quote:  cmp #QUOTE
            bne try_binary
            jsr CharGet
            sta SP_OPERAND
            jsr CharGet
            cmp #QUOTE
            bne AsmError
            jmp test
try_binary: cmp #"%"
            bne AsmError
            jsr BinaryByte
            bcc AsmError
            ;sta SP_OPERAND     ; Storage to SP_OPERAND is done by Binary
            jmp test            
            
; Error Message
; Invalid opcode or formatting (ASSEMBLY)
; Failed boolean assertion (MISMATCH, borrowed from ROM)
AsmError:   lda #<AsmErrMsg     ; ?ASSMEBLY
            ldx #>AsmErrMsg     ;   ERROR
            bne show_err
MisError:   lda #<MISMATCH      ; ?MISMATCH
            ldx #>MISMATCH      ;   ERROR
show_err:   sta ERROR_PTR       ; Set the selected pointer
            stx ERROR_PTR+1     ;   ,,
            jsr Restore         ; Return zeropage workspace to original
            jmp CUST_ERR        ; And emit the error

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
            sta SP_OPERAND      ; Save the speculative operand
getop_r:    rts
            
; Hypothesis Test
; Search through the language table for each opcode and disassemble it using
; the opcode provided for the candidate instruction. If there's a match, then
; that's the instruction to assemble at the program counter. If Hypotest tries
; all the opcodes without a match, then the candidate instruction is invalid.
Hypotest:   jsr ResetLang       ; Reset language table
reset:      ldy #$06            ; Offset disassembly by 5 bytes for buffer match   
            sty IDX_OUT         ;   b/c output buffer will be "$00AC INST"
            lda #OPCODE         ; Write location to PC for hypotesting
            sta PRGCTR          ; ,,
            ldy #$00            ; Set the program counter high byte
            sty PRGCTR+1        ; ,,
            jsr NextInst        ; Get next instruction in 6502 table
            cmp #TABLE_END      ; If we've reached the end of the table,
            beq bad_code        ;   the assembly candidate is no good
            sta OPCODE          ; Store opcode to hypotesting location
            jsr DMnemonic       ; Add mnemonic to buffer
            ldy #$01            ; Addressing mode is at (LANG_PTR)+1
            lda (LANG_PTR),y    ; Get addressing mode to pass to DOperand
            pha
            jsr DOperand        ; Add formatted operand to buffer
            lda #$00            ; Add 0 delimiter to end of output buffer so
            jsr CharOut         ;  the match knows when to stop
            pla
            cmp #RELATIVE       ; If the addressing mode is or immeditate,
            beq test_sp         ;   test separately
            cmp #IMMEDIATE      ;   ,,
            beq test_sp         ;   ,,
            jsr IsMatch
            bcc reset
match:      jsr NextValue
            lda PRGCTR          ; Set the INSTSIZE location to the number of
            sec                 ;   bytes that need to be programmed
            sbc #OPCODE         ;   ,,
            sta INSTSIZE        ;   ,,
            jmp RefreshPC       ; Restore the program counter to target address
test_sp:    lda #$0a            ; Handle speculative operands here; set
            sta IDX_OUT         ;   a stop after four characters in output
            jsr IsMatch         ;   buffer and check for a match
            bcc reset          
            lda SP_OPERAND      ; If the instruction matches, move the
            sta OPERAND         ;   speculative operand to the working operand
            jmp match           ; Treat this like a regular match from here
bad_code:   clc                 ; Clear carry flag to indicate failure
            rts
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; MEMORY DUMP COMPONENT
; https://github.com/Chysn/wAx/wiki/3-Memory-Dump
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Memory:     ldy #$00
-loop:      lda (PRGCTR),y
            sta CHARDISP,y
            jsr Hex
            iny
            cpy #$04
            beq show_char
            jsr Space
            jmp loop       
show_char:  lda #RVS_ON         ; Reverse on for the characters
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
            inc PRGCTR
            bne next_char
            inc PRGCTR+1
next_char:  iny
            cpy #04
            bne loop            
            rts
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; BINARY DUMP COMPONENT
; https://github.com/Chysn/wAx/wiki/3-Memory-Dump
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
BinaryDisp: ldx #$00            ; Get the byte at the program counter
            lda (PRGCTR,x)      ; ,,
            sta SP_OPERAND      ; Store byte for binary conversion
            lda #%10000000      ; Start with high bit
-loop:      pha
            bit SP_OPERAND
            beq is_zero
            lda #RVS_ON
            jsr CharOut
            lda #"1"
            jsr CharOut
            lda #RVS_OFF
            .byte $3c           ; TOP (skip word)
is_zero:    lda #"0"
            jsr CharOut
            pla
            lsr
            bne loop
            jsr Space
            ldx #$00
            lda (PRGCTR,x)
            jsr Hex
            jmp NextValue

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; ASSERTION TESTER COMPONENT
; https://github.com/Chysn/wAx/wiki/8-Assertion-Tester 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Tester:     ldy #$00
-loop:      jsr Buff2Byte
            bcc test_r          ; Bail out on the first non-hex byte
            cmp (PRGCTR),y
            bne test_err      
            iny
            cpy #$04
            bne loop
test_r:     rts
test_err:   jmp MisError

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; BREAKPOINT COMPONENTS
; https://github.com/Chysn/wAx/wiki/7-Breakpoint-Manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetBreak:   php
            jsr ClearBP         ; Clear the old breakpoint, if it exists
            plp                 ; If no breakpoint is chosen (e.g., if ! was)
            bcc SetupVec        ;   by itself), just clear the breakpoint
            lda PRGCTR          ; Add a new breakpoint at the program counter
            sta BREAKPOINT      ; ,,
            lda PRGCTR+1        ; ,,
            sta BREAKPOINT+1    ; ,,
            ;ldy #$00           ; (Y is already 0 from ClearBP)
            lda (PRGCTR),y      ; Stash it in the Breakpoint data structure,
            sta BREAKPOINT+2    ;   to be restored on the next break
            tya                 ; Write BRK to the breakpoint location
            sta (PRGCTR),y      ;   ,,
            lda #CRSRUP         ; Cursor up to overwrite the command
            jsr CHROUT          ; ,,
            ldx #$01            ; List a single line for the user to review
            jsr ListLine        ; ,,

; Set Up Vectors
; Used by installation, and also by the breakpoint manager                    
SetupVec:   lda #<main          ; Intercept GONE to process wedge
            sta IGONE           ;   tool invocations
            lda #>main          ;   ,,
            sta IGONE+1         ;   ,,
            lda #<Break         ; Set the BRK interrupt vector
            sta CBINV           ; ,,
            lda #>Break         ; ,,
            sta CBINV+1         ; ,,
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
            rts

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
            lda #RVS_ON         ; Reverse on for the breakpoint
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
            tya                 ; ,,
            sta (CHARAC),y      ; ,,
enable_r:   rts
             
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; REGISTER COMPONENT
; https://github.com/Chysn/wAx/wiki/5-Register-Editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Register:   bcc register_r      ; Don't set Y and X if they're not provided
            lda PRGCTR+1        ; Two bytes are already set in the program
            sta YREG            ;   counter. These are Y and X
            lda PRGCTR          ;   ,,
            sta XREG            ;   ,,
            jsr Buff2Byte       ; Get a third byte to set Accumulator
            sta ACC             ;   ,,
            jsr Buff2Byte       ; Get a fourth byte to set Processor Status
            sta PROC            ;   ,,
register_r: rts
                                                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; SUBROUTINE EXECUTION COMPONENT
; https://github.com/Chysn/wAx/wiki/6-Subroutine-Execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Execute:    pla                 ; Get rid of the return address to Return, as
            pla                 ;   it will not be needed (see BRK below)
            lda PRGCTR          ; Set the temporary INT storage to the program
            sta SYS_DEST        ;   counter. This is what SYS uses for its
            lda PRGCTR+1        ;   execution address, and I'm using that
            sta SYS_DEST+1      ;   system to borrow saved Y,X,A,P values
            jsr SetupVec        ; Make sure the BRK handler is enabled
            php                 ; Store P to preserve Carry flag
            jsr Restore         ; Restore zeropage workspace
            plp                 ; The Carry flag indicates that no valid address
            bcc ex_brk          ;   was provided; go to BRK if it was not
            jsr SYS             ; Call BASIC SYS, but a little downline
                                ;   This starts SYS at the register setup,
                                ;   leaving out the part that adds a return
                                ;   address to the stack. This omitted part
                                ;   sends BASIC's SYS to a second half, which
                                ;   updates the saved register values. I want
                                ;   those values to remain as they are, for
                                ;   repeat testing. To change this behavior,
                                ;   you would do two things 1) Set the value
                                ;   of the SYS label to $e127, and 2) Add
                                ;   lda ACC right after jsr SYS, because the
                                ;   second half of SYS messes with A, and you
                                ;   want the BRK interrupt to get it right.
ex_brk:     brk                 ; Trigger the BRK handler
           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; MEMORY SAVE COMPONENT
; https://github.com/Chysn/wAx/wiki/9-Memory-Save
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MemSave:    bcc save_err        ; Bail if the address is no good
            jsr Buff2Byte       ; Convert 2 characters to a byte   
            bcc save_err        ; Fail if the byte couldn't be parsed
            sta WORK+1          ; Save to the PRGCTR high byte
            jsr Buff2Byte       ; Convert next 2 characters to byte
            bcc save_err        ; Fail if the byte couldn't be parsed
            sta WORK            ; Save to the PRGCTR low byte
            jsr DiskSetup       ; SETLFS, get filename length, etc.  
            ldx #<INBUFFER+8    ; ,,
            ldy #>INBUFFER+8    ; ,,
            jsr SETNAM          ; ,,
            lda #PRGCTR         ; Set up SAVE call
            ldx WORK            ; ,,
            ldy WORK+1          ; ,,
            jsr SAVE            ; ,,
            bcs DiskError
            lda #$42            ; Close the file
            jsr CLOSE           ; ,,
            jsr Restore
            jmp Linefeed
save_err:   jsr Restore
            jmp SYNTAX_ERR      ; To ?SYNTAX ERROR      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; MEMORY LOAD COMPONENT
; https://github.com/Chysn/wAx/wiki/9-Memory-Save-Load
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MemLoad:    lda #$00            ; Reset the input buffer index because there's
            sta IDX_IN          ;   no address for this command
            jsr DiskSetup       ; SETLFS, get filename length, etc.
            ldx #<INBUFFER      ; Set location of filename
            ldy #>INBUFFER      ; ,,
            jsr SETNAM          ; ,,
            lda #$00            ; Perform Load
            ;ldx #$ff           ; ,, (X and Y don't matter because the secondary
            ;ldy #$ff           ; ,, address indicates use of header)
            jsr LOAD            ; ,,
            bcs DiskError
            lda #$42            ; Close the file
            jsr CLOSE           ; ,,
            jsr Restore
            jmp Linefeed
        
; Disk Setup
; Clear breakpoint, set up logical file, get filename length, return in A
; for call to SETNAM            
DiskSetup:  jsr ClearBP         ; Clear breakpoint
            lda #$42            ; Set up logical file
            ldx #DEVICE         ; ,,
            ldy #$01            ; ,, Specify use of header for address
            jsr SETLFS          ; ,,
            ldy #$00
-loop:      jsr CharGet
            beq setup_r
            iny
            cpy #$08
            bne loop            
setup_r:    tya
            rts

; Show System Disk Error            
DiskError:  pha
            jsr Restore
            pla
            jmp ERROR_NO            
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
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
Lookup:     sta INSTDATA        ; INSTDATA is the found opcode
            jsr ResetLang       ; Reset the language table reference
-loop:      jsr NextInst        ; Get the next 6502 instruction in the table
            cmp #TABLE_END      ; If we've reached the end of the table,
            beq not_found       ;   then the instruction is invalid
            cmp INSTDATA        ; If the instruction doesn't match the opcode,
            bne loop            ;   keep searching.
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
; to the following instruction. The opcode is returned in A.
NextInst:   lda #$02            ; Each language entry is two bytes. Advance to
            clc                 ;   the next entry in the table
            adc LANG_PTR        ;   ,,
            sta LANG_PTR        ;   ,,
            bcc ch_mnem         ;   ,,
            inc LANG_PTR+1      ;   ,,
ch_mnem:    ldy #$01            ; Is this entry an instruction record?
            lda (LANG_PTR),y    ; ,,
            and #$01            ; ,,
            beq adv_lang_r      ; If it's an instruction, return
            lda (LANG_PTR),y    ; Otherwise, set the mnemonic in the workspace
            sta MNEM+1          ;   as two bytes, five bits per character for
            dey                 ;   three characters. See the 6502 table for
            lda (LANG_PTR),y    ;   a description of the data encoding
            sta MNEM            ;   ,,
            jmp NextInst        ; Go to what should now be an instruction
adv_lang_r: ldy #$00            ; When an instruction is found, set A to its
            lda (LANG_PTR),y    ;   opcode and return
            rts
            
; Get Character
; Akin to CHRGET, but scans the INBUFFER, which has already been detokenized            
CharGet:    ldx IDX_IN
            lda INBUFFER,x
            php
            inc IDX_IN
            plp
            rts             
            
; Buffer to Byte
; Get two characters from the buffer and evaluate them as a hex byte
Buff2Byte:  jsr CharGet
            jsr Char2Nyb
            bcc not_found       ; See Lookup subroutine above
            asl                 ; Multiply high nybble by 16
            asl                 ;   ,,
            asl                 ;   ,,
            asl                 ;   ,,
            sta WORK
            jsr CharGet
            jsr Char2Nyb
            bcc buff2_r         ; Clear Carry flag indicates invalid hex
            ora WORK            ; Combine high and low nybbles
            ;sec                ; Set Carry flag indicates success
buff2_r:    rts
       
; Is Buffer Match            
; Does the input buffer match the output buffer?
; Carry is set if there's a match, clear if not
IsMatch:    ldy #$06            ; Offset for character after address
-loop:      lda OUTBUFFER,y     ; Compare the assembly with the disassembly
            cmp INBUFFER-2,y    ;   in the buffers
            bne not_found       ; See Lookup subroutine above
            iny
            cpy IDX_OUT
            bne loop            ; Loop until the buffer is done
            sec                 ; This matches; set carry
            rts

; Character to Nybble
; A is the character in the text buffer to be converted into a nybble
Char2Nyb:   cmp #"9"+1          ; Is the character in range 0-9?
            bcs not_digit       ; ,,
            cmp #"0"            ; ,,
            bcc not_digit       ; ,,
            sbc #"0"            ; If so, nybble value is 0-9
            rts
not_digit:  cmp #"F"+1          ; Is the character in the range A-F?
            bcs not_found       ; See Lookup subroutine above
            cmp #"A"         
            bcc not_found       ; See Lookup subroutine above
            sbc #"A"-$0a        ; The nybble value is 10-15
            rts
            
; Next Program Counter
; Advance Program Counter by one byte, and return its value
NextValue:  inc PRGCTR
            bne next_r
            inc PRGCTR+1
next_r:     ldx #$00
            lda (PRGCTR,x)
            rts

; Commonly-Used Characters
CloseParen: lda #")"
            .byte $3c           ; TOP (skip word)
Comma:      lda #","
            .byte $3c           ; TOP (skip word)
Space:      lda #" "
            .byte $3c           ; TOP (skip word)
HexPrefix:  lda #"$"
            jmp CharOut
 
Linefeed:   lda #LF
            jmp CHROUT 
            
; Write hexadecimal character
Hex:        pha                 ; Hex converter based on from WOZ Monitor,
            lsr                 ;   Steve Wozniak, 1976
            lsr
            lsr
            lsr
            jsr prhex
            pla
prhex:      and #$0f
            ora #"0"
            cmp #"9"+1
            bcc echo
            adc #$06
echo:       jmp CharOut

; Get Binary Byte
; Return in A     
BinaryByte: lda #$00
            sta SP_OPERAND
            lda #%10000000
-loop:      pha
            jsr CharGet
            tay
            cpy #"1"
            bne zero
            pla
            pha
            ora SP_OPERAND
            sta SP_OPERAND
            jmp next_bit
zero:       cpy #"0"
            bne bad_bin
next_bit:   pla
            lsr
            bne loop
            lda SP_OPERAND
            sec
            rts
bad_bin:    pla
            clc
            rts 
 
; Show 8-bit Parameter           
Param_8:    jsr HexPrefix
            jsr NextValue 
            jmp Hex            
            
; Show 16-Bit Parameter            
Param_16:   jsr HexPrefix
            jsr NextValue 
            pha
            jsr NextValue 
            jsr Hex
            pla
            jmp Hex

; Character to Output
; Add the character in A to the outut byffer            
CharOut:    sta CHARAC          ; Save temporary character
            tya                 ; Save registers
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
Transcribe: jsr CHRGET          ; Get character from input buffer
            cmp #$00            ; If it's 0, then quit transcribing and return
            beq xscribe_r       ; ,,
            cmp #QUOTE          ; If a quote is found, modify CHRGET so that
            bne ch_token        ;   spaces are no longer filtered out
            lda #$06            ; $0082 BEQ $0073 -> BEQ $008a
            sta $83             ; ,,
            lda #QUOTE          ; Put quote back so it can be added to buffer
ch_token:   cmp #$80            ; Is the character in A a BASIC token?
            bcc x_add           ; If it's not a token, just add it to buffer
            ldy $83             ; If it's a token, check the CHRGET routine
            cpy #$06            ;  and skip detokenization if it's been
            beq x_add           ;  modified.
            jsr Detokenize      ; Detokenize and continue transciption
            jmp Transcribe      ; ,, (Carry is always set by Detokenize)
x_add:      jsr AddInput        ; Add the text to the buffer
            jmp Transcribe      ; (Carry is always set by AddInput)
xscribe_r:  jmp AddInput        ; Add the final zero, and fix CHRGET...

; Add Input
; Add a character to the input buffer and advance the counter
AddInput:   ldx IDX_IN
            cpx #$16            ; Wedge lines are limited to the physical
            bcs add_r           ;   line length
            sta INBUFFER,x
            inc IDX_IN
add_r:      rts
           
; Detokenize
; If a BASIC token is found, explode that token into PETSCII characters 
; so it can be disassembled. This is based on the ROM uncrunch code around $c71a
Detokenize: ldy #$65
            tax                 ; Copy token number to X
get_next:   dex
            beq explode         ; Token found, go write
-loop       iny                 ; Else increment index
            lda KEYWORDS,y      ; Get byte from keyword table
            bpl loop            ; Loop until end marker
            bmi get_next
explode:    iny                 ; Found the keyword; get characters from
            lda KEYWORDS,y      ;   table
            bmi last_char       ; If there's an end marker, mask byte and
            jsr AddInput        ;   add to input buffer
            bne explode
last_char:  and #$7f            ; Take out bit 7 and
            jmp AddInput        ;   add to input buffer
 
; Print Buffer
; Add a $00 delimiter to the end of the output buffer, and print it out           
PrintBuff:  lda #$00            ; End the buffer with 0
            jsr CharOut         ; ,,
            lda #<OUTBUFFER     ; Print the line
            ldy #>OUTBUFFER     ; ,,
            jsr PRTSTR          ; ,,
            lda #RVS_OFF        ; Reverse off after each line
            jsr CHROUT          ; ,,
            jmp Linefeed
            
; Prompt for Next Line
; X should be set to the number of bytes the program counter should be
; advanced
Prompt:     jsr DirectMode      ; If a tool is in Direct Mode, increase
            bne prompt_r        ;   the PC by the size of the instruction
            tya                 ;   and write it to the keyboard buffer (by
            sta IDX_OUT         ;   way of populating the output buffer)
            lda TOOL_CHR        ;   ,,
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
            lda #CRSRRT         ;   ,,
            jsr CharOut         ;   ,,
            ;ldy #$00           ; (Y is still $00 from above)
-loop:      lda OUTBUFFER,y     ; Copy the output buffer into KEYBUFF, which
            sta KEYBUFF,y       ;   will simulate user entry
            iny                 ;   ,,
            cpy #$06            ;   ,,
            bne loop            ;   ,,
            sty KBSIZE          ; Setting the buffer size will make it go
prompt_r:   rts            
                            
; In Direct Mode
; If the wAx tool is running in Direct Mode, the Zero flag will be set
DirectMode: ldy CURLIN+1
            iny
            rts     
                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ToolTable contains the list of tools and addresses for each tool
ToolTable:	.byte T_DIS,T_ASM,T_MEM,T_REG,T_EXE,T_BRK,T_TST,T_SAV,T_LOA,T_BIN
ToolAddr_L: .byte <List-1,<Assemble-1,<List-1,<Register-1,<Execute-1
            .byte <SetBreak-1,<Tester-1,<MemSave-1,<MemLoad-1,<List-1
ToolAddr_H: .byte >List-1,>Assemble-1,>List-1,>Register-1,>Execute-1
            .byte >SetBreak-1,>Tester-1,>MemSave-1,>MemLoad-1,>List-1

; Text display tables                      
Intro:      .asc LF,"WAX ON",$00
Registers:  .asc LF,LF,"*Y: X: A: P: S: PC::",LF,";",$00
AsmErrMsg:  .asc "ASSEMBL",$d9

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
            .byte $0a,$b0       ; * ASL accumulator
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
            .byte $4a,$b0       ; * LSR accumulator
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
            .byte $2a,$b0       ; * ROL accumulator
            .byte $26,$70       ; * ROL zeropage
            .byte $36,$80       ; * ROL zeropage,X
            .byte $2e,$40       ; * ROL absolute
            .byte $3e,$50       ; * ROL absolute,X
            .byte $93,$e5       ; ROR
            .byte $6a,$b0       ; * ROR accumulator
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
Expand:     .byte TABLE_END,$00 ; End of 6502 table