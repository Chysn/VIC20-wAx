   10 rem implied
   11 $1800 "iny"
   20 rem acc #1
   21 $1801 "ror a"
   30 rem acc #2
   31 $1802 "asl"
   40 rem absolute
   41 $1803 "jsr $ffd2"
   50 rem absolute,x
   51 $1806 "sta $1e00,x"
   60 rem absolute,y
   61 $1809 "lda $8000,y"
   70 rem zeropage
   71 $180c "bit $a2"
   80 rem zeropage,x
   81 $180e "cmp $00,x"
   90 rem zeropage,y
   91 $1810 "ldx $fa,y"
  110 rem immediate
  111 $1812 "lda #$2d"
  120 rem indirect
  121 $1814 "jmp ($0306)"
  130 rem indirect,x
  131 $1817 "sta ($1e,x)
  140 rem indirect,y
  141 $1819 "cmp ($55),y"
  150 rem relative (+)
  151 $181b "bcc $181e"
  160 rem relative (-)
  161 $181d "beq $1814"
  170 rem done!
  171 $181f "rts"
  200 c=0
  210 for i = 0 to 31
  220 c=c+peek(6144+i)
  230 next i
  240 if c=3666 then 260
  250 print "failure!":end
  260 print "success!":end

