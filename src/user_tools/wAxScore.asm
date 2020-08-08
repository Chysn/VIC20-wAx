; wAxScore Plug-In
; Compose super-compact musical scores in the wAx ecosystem
;
; 'addr [R]
; Where addr is the address of the score data. If R is included after the
; address, the wAxScore will be in Record mode. Otherwise, wAxScore will
; play from the data at the specified address.
;
; *** KEY BINDINGS FOR RECORDING ***
;
; Notes    2 3   5 6 7
;         Q W E R T Y U I
;         c d e f g a b c'
; Rests   SPACE BAR
; Durations
;         f1 - Eighth Note
;         f3 - Quarter Note
;         f5 - Half Note
;         f7 - Whole Note
;          . - Dot the current duration (may be done multiple times)
; Effects RETURN (for an effect placeholder, see below)
; Undo    Left-Arrow
; Finish  STOP KEY
;
; *** THE WAXSCORE FORMAT ***
; 
; wAxScore is a simple format for one voice. Each byte contains duration
; and degree data. The high nybble is the length:
;   1000nnnn = Whole Note
;   0100nnnn = Half Note
;   0010nnnn = Quarter Note
;   0001nnnn = Eighth Note
;
; Note durations can be dotted by setting the bit to the right. For example,
; 0011 represents a dotted quarter note.
;
; The low nybble is the note number of a diatonic degree from 1 to 13. wAxScore
; format does not actually specify the temperament; that's determined by the
; table that defines relationships between diatonic degrees and note values.
;
; A low nybble value of zero indicates a rest for the specified duration. If
; low and high nybble are both 0 ($00), it indicates the end of the score.
; 
; A low nybble value of $f indicates that the high nybble contains an effect.
; It is optional for wAxScore players to interpret effects, but they must treat
; effects as zero-duration. Effects include the following:
;   * $0    No effect (effect placeholder)
;   * $1    Octave Up
;   * $2    Octave Down
;   * $3    Legato On (voice is not set to 0 at end of Play)
;   * $4    Legato Off
;   * $5    Crescendo (volume ++)
;   * $6    Decrescendo (volume --)
;   * $7    Accellerando (tempo ++)
;   * $8    Ritardando (tempo --)
;   * $9-$f User effects
; Note that the player in this plug-in does not implement any of these effects
; (except for $0), but effects may be implemented in IRQ players based on need
; or available memory. Pressing RETURN during Record will enter $0f for an 
; effect placeholder, which you can fill in with wAx's data entry tools.
;
; wAxScore players may be developed that can play multiple voices from multiple
; scores simultaneously.
;
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
IncAddr     = $700c             ; Get the EA byte and advance EA by one
IncPC       = $700f             ; Increment persistent counter
Lookup      = $7012             ; Lookup 6502 instruction with operand in A
PrintBuff   = $7015             ; Flush output buffer to screen
ResetIn     = $7018             ; Reset input buffer index
ResetOut    = $701b             ; Reset output buffer index
ShowAddr    = $701e             ; Write effective address to output buffer
ShowPC      = $7021             ; Write persistent counter to output buffer

; System Resources
TIME_L      = $a2               ; Jiffy counter low
VOICEH      = $900c             ; High sound register
VOICEM      = $900b             ; Mid sound register
VOICEL      = $900a             ; Low sound register
NOISE       = $900d             ; Noise register
VOLUME      = $900e             ; Sound volume register/aux color

; User Tool Storage
DURATION    = $0247             ; Current duration

; Constants
WHOLE       = $80               ; Whole note
HALF        = $40               ; Half note
QUARTER     = $20               ; Quarter note
EIGHTH      = $10               ; Eighth note

* = $7e00
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAIN PLUG-IN PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Main:       bcs addr_ok         ; Bail if no valid address was provided
            rts                 ; ,,
addr_ok:    lda #$08            ; Set volume
            sta VOLUME          ; ,,
            jsr CharGet         ; Check for Record mode
            cmp #"R"            ; ,,
            beq Record          ; ,,
            jmp Player          ; Otherwise, play at address

; Record Score
; Duration  f1 (Eighth)
;           f3 (Quarter)
;           f5 (Half)
;           f7 (Whole)
; Notes     Q,2,W,3,E,R,5,T,6,Y,7,U,I
; Rests     Space
; Back      Back Arrow
; End       STOP
Record:     lda #QUARTER        ; Starting duration
            sta DURATION        ; ,,
-loop:      jsr GetKey          ; Get keypress
            jsr IsNote          ; Is it a note or rest?
            bcs add_note
            cmp #8              ; Back arrow
            beq undo
            cmp #15             ; RETURN
            beq effect
            cmp #39             ; f1 - Eighth note
            beq set8            ; ,,
            cmp #47             ; f3 - Quarter note
            beq set4            ; ,,
            cmp #55             ; f5 - Half note
            beq set2            ; ,,
            cmp #63             ; f7 - Whole note
            beq set1            ; ,,
            cmp #37             ; Period - Dot the note
            beq dot             ; ,,
            cmp #24             ; STOP
            bne loop
            lda #$00            ; Clear the keyboard buffer so it doesn't
            sta $c6             ;   flush to the screen on exit
            ldx #$00            ; Add $00 to indicate end-of-score
            sta (EFADDR,x)      ; ,,
            jsr ShowData        ; ,,
            jsr IncAddr         ; Increment effective address
            jmp EAtoPC          ; Update the persistent counter and exit
; Go back to previous note to correct it                        
undo:       lda EFADDR
            sec
            sbc #$01
            sta EFADDR
            bcs show_prev
            dec EFADDR+1
show_prev:  jsr ResetOut        ; Show the previous address to verify back
            lda #"U"            ;   action
            jsr CharOut         ;   ,,
            jsr ShowAddr        ;   ,,
            jsr PrintBuff       ;   ,,
            jsr EAtoPC
            jmp loop
; Add an effect placeholder            
effect:     lda #$0f            ; Enter an effect placeholder into
            jmp add_note+3      ;   memory
; Set a note duration            
set8:       lda #EIGHTH
            .byte $3c           ; TOP (skip word)
set4:       lda #QUARTER
            .byte $3c           ; TOP (skip word)
set2:       lda #HALF
            .byte $3c           ; TOP (skip word)
set1:       lda #WHOLE
            sta DURATION
            bne loop
; Dot the current duration
dot:        lda DURATION        ; Take the current duration
            lsr                 ;   and shift it right
            ora DURATION        ; Then add the duration back to dot note
            and #$f0            ; Keep the duration in the high nybble
            sta DURATION
            jmp loop
; Add a note            
add_note:   ora DURATION
            ldx #$00
            sta (EFADDR,x)
            jsr ShowData        ; Show note data
            jsr PlayNote        ; Play the note at the selected duration
            jsr IncAddr         ; Increment the address
            jsr EAtoPC          ; Update the persistent counter and go back
            jmp loop

; Simple wAxScore player
Player:     jsr ShowData        ; Show the note about to be played
            jsr PlayNote        ; Play it
            php
            jsr IncAddr         ; Increment the address
            jsr EAtoPC          ;   ,,
            plp
            bcc player_r        ; If end-of-score marker, then end
            lda $c5             ; Check STOP key
            cmp #24             ; ,,
            beq player_r        ; ,,
            jmp Player          ; Play next note
player_r:   lda #$00            ; Turn off the playing voice
            sta VOICEH          ; ,,
            rts            
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Get Key
; With debounce, and return as A
GetKey:     lda $c5             ; Make sure any previous keys are released first
            cmp #$40            ; ,,
            bne GetKey          ; ,,
-loop:      lda $c5             ; Now wait for an actual keypress start
            cmp #$40            ; ,,
            beq loop            ; ,,
            rts

; Is Note or Rest
; Set Carry and return degree in A if it's a note or rest
; Clear Carry and return original key code in A if it's something else
IsNote:     pha                 ; Save the original keypress
            ldx #$0d            ; Loop through 13 notes and 1 rest key to see
-loop:      cmp Degree,x        ;   if it matches one of the diatonic degrees
            beq found_deg       ;   ,,
            dex                 ;   ,,
            bpl loop            ;   ,,
            pla                 ; Loop ends without finding a note or rest key,
            clc                 ;   so pull the original key and send back to
            rts                 ;   look for other key presses
found_deg:  pla                 ; Found degree; pull original key and discard
            txa                 ; A is now the diatonic degree
            sec                 ; Set Carry to indicate note or rest was found
            rts

; Delay A Jiffies
Delay:      clc
            adc TIME_L
-loop:      cmp TIME_L
            bne loop
            rts

; Play Note
; At the effective address
; Set Carry if the note was played successfully
; Clear Carry if this is the end of the score
PlayNote:   ldx #$00
            lda (EFADDR,x)
            beq end_score
            pha
            and #$0f            ; Mask away the duration
            tax                 ; X is the diatonic degree
            lda Note,x
            tay
            pla
            cpx #$0f            ; This is an effect, so do nothing, but return
            beq play_r          ;   with Carry set
            and #$f0            ; Mask away the degree, leaving A as the
                                ;   duration. Now, with a quarter note having a
                                ;   value of 32, simply treating that as a
                                ;   jiffy counter would give us a tempo of
                                ;   112 beats per minute, which is probably
                                ;   okay in this system. The IRQ player will
                                ;   give the developer more control over
                                ;   tempo in a production environment.
            ldx TIME_L          ; Sync to the start of the next jiffy, within a
-loop:      cpx TIME_L          ;   few cycles
            beq loop            ;   ,,
            sty VOICEH          ; Play the voice
            jsr Delay           ; Delay A jiffies, see above for why
            lda #$00            ; Stop the voice
            sta VOICEH          ; ,,
play_r:     sec
            rts
end_score:  clc                 ; Nothing left to play; clear Carry and return
            rts                 ; ,,

; Show Note Data
; At the effective address
ShowData:   jsr ResetOut        ; Show the data as it's entered
            lda #"@"            ;   as a wAx data entry command
            jsr CharOut         ;   ,,
            jsr ShowAddr        ;   ,,
            lda #":"            ;   ,,
            jsr CharOut         ;   ,,
            ldx #$00            ;   ,,
            lda (EFADDR,x)      ;   ,,
            jsr Hex             ;   ,,
            jmp PrintBuff       ;   ,,
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Key to Degree         2    3       5    6     7
;                SPC Q     W   E  R    T     Y    U  I
Degree:     .byte 32,48,56,9,1,49,10,2,50,58,11,3,51,12

; Degree to Note Value
; From Personal Computing on the VIC-20, Appendix F
Note:       .byte 0,135,143,147,151,159,163,167,175,179,183,187,191,195
