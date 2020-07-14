; wAx API
EFADDR      = $a6       ; Effective Address
X_PC        = $03       ; External Persistent Counter
Buff2Byte   = $6ab6     ; Get 8-bit hex number from input buffer to A
CharGet     = $6aac     ; Get character from input buffer to A
CharOut     = $6b1c     ; Write character in A to output buffer
Hex         = $6b30     ; Write value in A to output buffer as 8-bit hex number
IncAddr     = $6af6     ; Increment Effective Address, store value in A
IncPC       = $6b01     ; Increment Persistent Counter
PrintBuff   = $6c6e     ; Flush output buffer to screen
ResetIn     = $6c3f     ; Reset input buffer index
ResetOut    = $6c3a     ; Reset output buffer index
ShowAddr    = $6b70     ; Write 16-bit Effective Address to output buffer
ShowPC      = $6b7a     ; Write 16-bit Persistent Counter to output buffer

