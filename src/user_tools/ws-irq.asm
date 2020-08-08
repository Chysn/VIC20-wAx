; This is a sample IRQ player for wAxScore data

SCORE       = $1800
POSITION    = $f9               ; Current score position (2 bytes)
PLAY_FLAG   = $033c             ; Bit 7 set when playing     
COUNTDOWN   = $033d             ; Countdown to next note
TEMPO       = $033e             ; Tempo in jiffies per 8th note 
VOICE       = $900b             ; Mid sound register

* = $0340
; Sample Setup
Sample:     sei
            lda #<ISR
            sta $0314
            lda #>ISR
            sta $0315
            cli
            jsr wsReset
            lda #$03
            sta TEMPO
            jsr wsPlay
            rts

; Sample ISR            
ISR:        jsr wsService
            jmp $eabf

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; WAXSCORE IRQ PLAYER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Reset Score to Start
wsReset:    lda #<SCORE
            sta POSITION
            lda #>SCORE
            sta POSITION+1
            lda #$00
            sta COUNTDOWN
            rts

; Begin Playing
wsPlay:     lda #$80
            sta PLAY_FLAG
            rts
            
; Stop Playing
wsStop:     lda #$00
            sta PLAY_FLAG
            sta VOICE
            rts
            
; Service Routine           
wsService:  bit PLAY_FLAG
            bpl svc_r
            lda COUNTDOWN
            beq fetch_note
            dec COUNTDOWN
            rts
fetch_note: ldx #$00
            stx COUNTDOWN       ; Initialize countdown
            lda (POSITION,x)
            beq eos             ; End of score
            tay                 ; Y holds the full note data
            and #$0f            ; Mask away the duration
            cmp #$0f            ; Is this a effect?
            beq effect
            tax                 ; X is the note table index
            lda Note,x          ; A is the note value
            sta VOICE           ; Set the voice register
            tya
            and #$f0            ; Mask away the note index
            lsr
            lsr
            lsr
            lsr
            ldy TEMPO
-loop:      clc
            adc COUNTDOWN
            sta COUNTDOWN
            dey
            bne loop
            inc POSITION
            bne svc_r
            inc POSITION+1
svc_r:      rts
eos:        jsr wsReset
            rts       

; Apply Effect
; The effect index is in Y            
effect:     cpy #$00            ; Handle Placeholder
            bne ch_non_ph       ; ,,
            rts
ch_non_ph:  rts                 ; Implement other effects            

; Degree to Note Value
; From Personal Computing on the VIC-20, Appendix F
Note:       .byte 0,135,143,147,151,159,163,167,175,179,183,187,191,195