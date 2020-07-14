; wAx API
EFADDR      = $a6       ; Effective Address
X_PC        = $03       ; External Persistent Counter
Buff2Byte   = $6ab9     ; Get 8-bit hex number from input buffer to A
CharGet     = $6aaf     ; Get character from input buffer to A
CharOut     = $6b1f     ; Write character in A to output buffer
Hex         = $6b33     ; Write value in A to output buffer as 8-bit hex number
IncAddr     = $6af9     ; Increment Effective Address, store value in A
IncPC       = $6b04     ; Increment Persistent Counter
PrintBuff   = $6c71     ; Flush output buffer to screen
ResetIn     = $6c42     ; Reset input buffer index
ResetOut    = $6c3d     ; Reset output buffer index
ShowAddr    = $6b73     ; Write 16-bit Effective Address to output buffer
ShowPC      = $6b7d     ; Write 16-bit Persistent Counter to output buffer

