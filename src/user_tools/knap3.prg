 ��  @A6A5 �B;BREAKPT 4 @FAF9 �C;CODE ADDR >d �A200 Oi @�    BCC �R bn @�    LDY #$04 {s @�    LDA #$EA ; NOP �x @�    STA �K,Y �} @�    DEY �� @�    LDA #$00 ; BRK �� @�    STA �K,Y �� @�    DEY �� @� �@ LDA (�BL),Y �� @�    STA �K,Y 	� @�    DEY 	� @�    BPL �@ ,	� @�    INY D	� @�    LDA #$4C ;JMP Z	� @�    STA (�BL),Y h	� @�    INY {	� @�    LDA #�KL �	� @�    STA (�BL),Y �	� @�    INY �	� @�    LDA #�KH �	� @�    STA (�BL),Y �	� @�    LDY #$05 �	� @�    LDA #$4C ; JMP 
� @�    STA �K,Y 
� @�    INY '
� @�    LDA �BL 5
� @�    CLC H
� @�    ADC #$03 [
� @�    STA �K,Y i
� @�    INY {
� @�    LDA �BH �
@�    ADC #$00 �
	@�    STA �K,Y �
@�    LDA �BL �
@�    STA �CL �
@�    LDA �BH �
@�    STA �CH �
"@�    RTS ,@� �R ; � 1@�    LDY #$02 +6@� �@ LDA �K,Y A;@�    STA (�CL),Y O@@�    DEY `E@�    BPL �@ nJ@�    RTS �O@� �K ; KNAPSACK ��� 5,0 ��� 6,162   