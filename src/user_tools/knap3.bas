1 *-
3 @a6a5 -b;breakpt
6 @faf9 -c;code addr
100 *a200
105 @*    bcc -r
110 @*    ldy #$04
115 @*    lda #$ea ; nop
120 @*    sta -k,y
125 @*    dey
130 @*    lda #$00 ; brk
135 @*    sta -k,y
140 @*    dey
145 @* -@ lda (-bl),y
150 @*    sta -k,y
155 @*    dey
160 @*    bpl -@
165 @*    iny
170 @*    lda #$4c ;jmp
175 @*    sta (-bl),y
180 @*    iny
185 @*    lda #-kl
190 @*    sta (-bl),y
195 @*    iny
200 @*    lda #-kh
205 @*    sta (-bl),y
210 @*    ldy #$05
215 @*    lda #$4c ; jmp
220 @*    sta -k,y
225 @*    iny
230 @*    lda -bl
235 @*    clc
240 @*    adc #$03
245 @*    sta -k,y
250 @*    iny
255 @*    lda -bh
260 @*    adc #$00
265 @*    sta -k,y
270 @*    lda -bl
275 @*    sta -cl
280 @*    lda -bh
285 @*    sta -ch
290 @*    rts
300 @* -r ; restore
305 @*    ldy #$02
310 @* -@ lda -k,y
315 @*    sta (-cl),y
320 @*    dey
325 @*    bpl -@
330 @*    rts
335 @* -k ; knapsack
500 poke 5,0
505 poke 6,162