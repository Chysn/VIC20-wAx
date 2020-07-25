; wAx ML2BAS Tool
; 'start end+1 [R]
; Convert the 6502 code between start and end into a BASIC program in the
; current BASIC stage. The 6502 code will be appended to the existing program,
; with line numbers in increments of 5.
;
; The specified end address should be the byte after the last instruction
; in the program.
;
; If R is specified after the end address, uses the relocatable "hermit crab"
; syntax. Otherwise, uses absolute addresses.
;
; If the disassembly would extend beyond the end of the BASIC stage, the
; existing BASIC program (if any) is restored, and an OUT OF MEMORY error is
; thrown.

; wAx API
EFADDR      = $a6               ; Effective Address
X_PC        = $03               ; External Persistent Counter
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

Disasm      = $60fb             ; Disassemble to output buffer
Rechain     = $6a4f             ; Rechain BASIC program
DMnemonic   = $611e             ; Display mnemonic

; ML2BAS Workspace
LINE_NUM    = $0247             ; BASIC Line Number (2 bytes)
MODIFIER    = $0249             ; Relocate or 
FAIL_POINT  = $024a             ; BASIC program end restore point (2 bytes)
OUTBUFFER   = $0218             ; Output buffer (24 bytes)
IDX_OUT     = $ad               ; Buffer index - Output
RANGE_END   = $0254             ; End of range for Save and Copy (2 bytes)

*=$7b00
Main:       bcc error           ; Error if the first address is no good
            jsr Buff2Byte       ; Get high byte of range end
            bcc error           ; ,,
            sta RANGE_END+1     ; ,,
            jsr Buff2Byte       ; Get low byte of range end
            bcc error           ; ,,
            sta RANGE_END       ; ,,
            jsr CharGet         ; If there's an R at the end of the command,
            cmp #"R"            ;   use the "hermit crab" syntax instead of
            beq set_mod         ;   absolute addresses
            cmp #"H"            ; If there's an H at the end of the command,            
            beq set_mod         ;   this will create hex dump lines instead of
            lda #$00            ;   code
set_mod:    sta MODIFIER        ;   ,,
            sta FAIL_POINT+1    ; Initialize fail point high byte
            lda $2b             ; Set persistent counter with start of
            sta X_PC            ;   BASIC
            lda $2c             ;   ,,
            sta X_PC+1          ;   ,,
            lda #$64            ; Start at line 100 by default
            sta LINE_NUM        ; ,,
            lda #$00            ; ,,
            sta LINE_NUM+1      ; ,,
            jsr NextLink        ; Is there an existing BASIC program?
            bcc found_end       ; If no existing program,
            jsr FindEnd         ; Find the last line number
            jsr IncLine         ; Increment it by 5
            lda X_PC            ; Set fail point, which preserves the existing
            sta FAIL_POINT      ;   BASIC program if the ML2BAS process results
            lda X_PC+1          ;   in an out of memory condition.
            sta FAIL_POINT+1    ;   ,,
found_end:  lda MODIFIER        ; If the code is not relocatable, skip the
            beq Start           ;   PC setting up front
            jsr LinkBytes       ; Add link bytes to next line
            jsr LineNumber      ; Add line number to first line
            lda #$ac            ; Add PC set tool
            jsr AddByte         ; ,,
            jsr ResetOut        ; Add the start address to the output buffer
            jsr ShowAddr        ; ,,
            jsr AddBuffer       ; Add the output buffer to the BASIC line
            jsr EndLine         ; Finish the first BASIC line
            jmp Start           ; Start adding lines of 6502 code
error:      jmp $cf08           ; ?SYNTAX ERROR, warm start
            
Start:      lda EFADDR+1        ; Is the effective address in 
            cmp RANGE_END+1     ;   the code range?
            bcc in_range        ;   ,,
            lda EFADDR          ;   ,,
            cmp RANGE_END       ;   ,,
            bcc in_range        ; If so, continue
done:       jsr EndProgram      ; Add $00,$00 to the the program
            jsr Rechain         ; Rechain BASIC program 
            jmp ($c002)         ; READY.
in_range:   jsr LinkBytes
            jsr LineNumber
            lda #"@"            ; Add the assemble tool
            jsr AddByte         ; ,,
            lda MODIFIER        ; If the user requested relocatable code,
            beq show_addr       ;   add the * instead of the address
            lda #$ac            ;   ,,
            jsr AddByte         ;   ,,
            jmp code_part       ;   ,,
show_addr:  jsr ResetOut        ; Add the current address to the BASIC line
            jsr ShowAddr        ; ,,
            jsr AddBuffer       ; ,,
code_part:  lda #" "            ; Space after address or hermit crab
            jsr AddByte         ; ,,
            jsr ResetOut        ; Reset output for the code portion
            lda MODIFIER        ; If the disassembly is in relocate mode,
            beq gen_code        ;   
            cmp #"H"            ;   check for hex dump modifier and
            beq HexDump         ;   handle that, if necessary. Otherwise, check
            jsr CheckRel        ;   for relative branch. If so, disassemble the
            bcs code2buff       ;   instruction as two bytes.
gen_code:   jsr Disasm          ; Disassemble code to empty output buffer and
code2buff:  jsr AddBuffer       ;   add it to the BASIC LINE
            jsr EndLine         ; End the line
            jmp Start           ; Check for the next line of code

; Hex Dump Line
; Add up to six hex bytes to the current BASIC line buffer
HexDump:    lda #$06            ; Reset a byte counter; we'll add up to six
            sta $08             ;   bytes per BASIC line
            lda #":"            ; Add a colon to specify hex entry
            jsr AddByte         ; ,,
-loop:      jsr IncAddr         ; Add the hex data to the buffer
            jsr Hex             ; ,,
            lda EFADDR+1        ; Is the effective address in 
            cmp RANGE_END+1     ;   the code range?
            bcc next_byte       ;   ,,
            lda EFADDR          ;   ,,
            cmp RANGE_END       ;   ,,
            bcs code2buff       ; If not, finish the line
next_byte:  dec $08
            lda $08
            bne loop
            beq code2buff

; Add Link Bytes
; We're not trying to keep track of the starting addresses of each line,
; because BASIC can do that.
LinkBytes:  lda #$ff            ; Add two $ff bytes to start the next
            jsr AddByte         ;   BASIC line. These will be set by the BASIC
            jsr AddByte         ;   rechain operation at the end of the build
            rts
 
; Add Line Number            
LineNumber: lda LINE_NUM        ; Add the current line number to the
            jsr AddByte         ;   BASIC line 
            lda LINE_NUM+1      ;   ,,
            jsr AddByte         ;   ,,
IncLine:    lda #$05            ; Increment the line number by 5
            clc                 ; ,,
            adc LINE_NUM        ; ,,
            sta LINE_NUM        ; ,,
            lda #$00            ; ,,
            adc LINE_NUM+1      ; ,,
            sta LINE_NUM+1      ; ,,
            rts    
 
; Add Byte
; Add the byte in Accumulator to the BASIC line            
AddByte:    pha
            ldx #$00
            sta (X_PC,x)
            jsr IncPC
            lda X_PC+1          ; Check memory for end of BASIC
            cmp $34             ; ,,
            bcc ok              ; ,,
            lda X_PC            ; ,,
            cmp $33             ; ,,
            bcs OutOfMem        ; If at limit of memory, then ERROR
ok:         pla
            rts        

; Perform NEW, then show Out of Memory Error
OutOfMem:   lda FAIL_POINT+1    ; Is there an existing program?
            bne restore         ; If so, restore it instead of NEW
            jsr $c642           ; Perform NEW
            jmp $c435           ; Out of Memory Error + Warm Start                        
restore:    lda FAIL_POINT      ; Reset the bytes at the previous
            sta $07             ;   program end address to $00,
            lda FAIL_POINT+1    ;   essentially reversing everything
            sta $08             ;   this process did, and the rechain
            ldy #$00            ;   the BASIC program so it's like
            tya                 ;   nothing ever happened.
            sta ($07),y         ;   ,,
            iny                 ;   ,, 
            sta ($07),y         ;   ,,
            jsr Rechain         ;   ,,
            jmp $c435           ; Out of Memory Error + Warm Start

; End Program / End Line
; End the BASIC line or program
EndProgram: jsr EndLine            
EndLine:    lda #$00
            jsr AddByte
            rts

; Add Output Buffer
; Paste output buffer into the BASIC program, without the ending $00            
AddBuffer:  ldy #$00
-loop:      lda OUTBUFFER,y
            jsr AddByte
            iny
            cpy IDX_OUT
            bne loop
buffer_out: rts 

; Find End of Program
FindEnd:    jsr NextLink        ; Get the next BASIC line location
            bcs get_line        ; If a line was found, advance line number and
            rts                 ;   link pointer and try again; else, return
get_line:   iny                 ; Get the line number and update it
            lda (X_PC),y        ;   it      
            sta LINE_NUM        ;   ,,
            iny                 ;   ,,
            lda (X_PC),y        ;   ,,
            sta LINE_NUM+1      ;   ,,
            lda $07             ; Get the next link pointer and update X_PC
            sta X_PC            ;   ,,
            lda $08             ;   ,,
            sta X_PC+1          ; Keep looking for the end
            jmp FindEnd

NextLink:   ldy #$00            ; Set locations $07 and $08 to the next
            lda (X_PC),y        ; BASIC line pointer. If both are $00, then
            sta $07             ; we're at the end of the BASIC program,
            iny                 ; otherwise, the BASIC program continues
            lda (X_PC),y        ; at the specified address
            sta $08             ; ,,
            sec                 ; Set Carry if the link isn't $00/$00
            lda $07             ; ,,
            bne next_r          ; ,,
            lda $08             ; ,,
            bne next_r          ; ,,
clc_r:      clc                 ; Otherwise clear it to indicate end of program
next_r:     rts
         
; Check Relative Instruction
; for relocatable byte syntax            
CheckRel:   ldx #$00            ; Check the instruction at the effective address
            lda (EFADDR,x)      ; ,,
            jsr Lookup          ; If it doesn't exist, exit
            bcc clc_r           ; ,,
            cmp #$c0            ; Is the instruction relative mode?
            bne clc_r           ; If not, exit
            lda #":"            ; Add a colon to indicate that bytes follow
            jsr CharOut         ; ,,
            jsr IncAddr         ; Add the instruction opcode to the buffer
            jsr Hex             ; ,,
            lda #" "            ; Add a space between the instruction and the
            jsr CharOut         ;   operand
            jsr IncAddr         ; Add the operand to the buffer
            jsr Hex             ; ,,
            lda #";"            ; Show the mnemonic for the instruction as
            jsr CharOut         ;   a comment, for the reader's benefit
            jsr DMnemonic       ;   ,,
            sec                 ; Set Carry to indicate that a relative
            rts                 ;   instruction was handled            
