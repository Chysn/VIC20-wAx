; wAx ML2BAS Tool
; 'start end+1 [R]
; Convert the 6502 code between start and end into a BASIC
; program in the current BASIC stage
;
; The end address should be the byte after the last instruction
; in the program
;
; If R is specified after the end address, use the relocatable
; "hermit crab" syntax. Otherwise, use absolute addresses

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
Rechain     = $6a50             ; Rechain BASIC program

; ML2BAS Workspace
LINE_NUM    = $0247             ; BASIC Line Number (2 bytes)
RELOCATE    = $0249             ; Use relocatable "hermit crab" syntax
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
            bne set_line        ;   absolute addresses
            lda #$ac            ;   ,,
            .byte $3c           ;   ,,
set_line:   lda #$00            ;   ,,
            sta RELOCATE        ;   ,,
            lda #$64            ; Start at line 100
            sta LINE_NUM        ; ,,
            lda #$00            ; ,,
            sta LINE_NUM+1      ; ,,
            lda $2b             ; Set persistent counter with start of
            sta X_PC            ;   BASIC
            lda $2c             ;   ,,
            sta X_PC+1          ;   ,,
            lda RELOCATE        ; If the code is not relocatable, skip the
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
            cmp RANGE_END+1     ;   the compare range?
            bcc in_range        ;   ,,
            lda EFADDR          ;   ,,
            cmp RANGE_END       ;   ,,
            bcc in_range        ; If so, check for byte match
done:       jsr EndProgram      ; Add $00,$00 to the the program
            jsr Rechain         ; Rechain BASIC program 
            jmp ($c002)         ; READY.
in_range:   jsr LinkBytes
            jsr LineNumber
            lda #"@"            ; Add the assemble tool
            jsr AddByte         ; ,,
            lda RELOCATE        ; If the user requested relocatable code,
            beq show_addr       ;   add the * instead of the address
            jsr AddByte         ;   ,,
            jmp space           ;   ,,
show_addr:  jsr ResetOut        ; Add the current address to the BASIC line
            jsr ShowAddr        ; ,,
            jsr AddBuffer       ; ,,
space:      lda #" "            ; And then a space
            jsr AddByte         ; ,,
            jsr ResetOut        ; Disassemble the current code to an
            jsr Disasm          ;   empty output buffer and then add it to
            jsr AddBuffer       ;   the BASIC LINE
            jsr EndLine         ; End the line
            jmp Start           ; Check for the next line of code

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
            lda #$05            ; Increment the line number by 5
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
OutOfMem:   jsr $c642           ; Perform NEW
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
