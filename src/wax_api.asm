; wAx API
EFADDR      = $a6               ; effective address
X_PC        = $03               ; External persistent counter
IDX_IN      = $ae               ; Input buffer index
IDX_OUT     = $ad               ; Output buffer index
Buff2Byte   = $7000             ; Get 8-bit hex number from input buffer to A
CharGet     = $7003             ; Get character from input buffer to A
CharOut     = $7006             ; Write character in A to output buffer
EAtoPC      = $7024             ; Copy effective address to persistent counter
Hex         = $7009             ; Write value in A to output buffer 8-bit hex
IncAddr     = $700c             ; Get byte at EA and increment EA by one
IncPC       = $700f             ; Increment persistent counter
Lookup      = $7012             ; Lookup 6502 instruction with operand in A
PrintBuff   = $7015             ; Flush output buffer to screen
ResetIn     = $7018             ; Reset input buffer index
ResetOut    = $701b             ; Reset output buffer index
ShowAddr    = $701e             ; Write effective address to output buffer
ShowPC      = $7021             ; Write persistent counter to output buffer