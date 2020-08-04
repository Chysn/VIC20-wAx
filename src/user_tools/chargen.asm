; wAx Character Tool
; '[addr]
; If no address is provided, clear the screen and draw a canvas for a custom
; character.
;
; If an address is provided, put the character in the canvas into memory at
; the specified address.
;
; Inside the canvas, the colon character is interpreted as 0, and anything
; else is interpreted as 1.
 
; wAx API
EFADDR      = $a6               ; Effective Address
IncAddr     = $700c             ; Increment Effective Address, store value in A

; Character generator workspace
CURBYTE     = $0247             ; Current byte value

* = $7d00
Main:       bcc Canvas
            lda #$00
            sta $07
            lda $0288
            sta $08
scanline:   lda #$00
            sta CURBYTE
            ldx #$80            ; Set bit 7
            ldy #$00
check:      lda ($07),y
            cmp #":"
            beq next
            txa
            ora CURBYTE
            sta CURBYTE
next:       iny
            txa
            lsr
            tax
            bne check
output:     ldy #$00
            lda CURBYTE
            sta (EFADDR),y
            jsr IncAddr
            lda $07
            cmp #$9a
            beq char_r
            lda #$16
            clc
            adc $07
            sta $07
            bcc scanline
char_r:     rts

; Draw Canvas
Canvas:     jsr $e55f           ; Clear screen
            ldy #$08
nextline:   ldx #$08
            lda #":"
-loop:      jsr $ffd2
            dex
            bne loop
            lda #$0d
            jsr $ffd2
            dey
            bne nextline
            rts
            
                        

            