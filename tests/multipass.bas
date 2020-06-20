   10 print "{up}  testing...     ";
  100 *-
  101 *1c00
  102 @* lda -0
  103 @* ldy -1
  104 @* ora -2
  105 @* and -3
  106 @* ldx #-4l
  107 @* ldy #-4h
  108 @* sta -5
  109 @* stx -6
  110 @* sty -7
  111 @* bcc -8
  111 @* lda (-9l,x)
  112 @* lda (-9h),y
  113 @* bne -t
  114 @* sta -t
  115 @* sty -tl
  116 @* ora -t
  120 @* -0 iny
  121 @* -1 dey
  122 @* -2
  123 @* -3
  124 @* -4
  125 @* -5
  126 @* -6 jsr $ffd2
  127 @* -7 clc
  128 @* -8 cld
  129 @* -9
  130 @* -t
  200 if peek(767) then 101
  500 =1c00 ad291cac2a1c0d2b
  501 =*    1c2d2b1ca22ba01c
  502 =*    8d2b1c8e2b1c8c2e
  502 =*    1c9014a130b11cd0
  503 =*    0f8d301c84300d30
  504 =*    1cc88820d2ff18d8
  999 print "{blue}[ok]{black}"
  1000 print "{blue}tests complete!"
  1005 new
