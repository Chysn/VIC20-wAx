   10 print "{up}  testing...     ";
  100 *
  101 @; test def
  102 @02a5 -a
  103 *1800 ; set pc
  104 @; test fwd ref
  105 @*    jsr -f
  106 @*    lda #->l
  107 @*    ldx #->h
  108 @*    bcc -2
  109 @* -f lda (-dl,x)
  110 @* -2
  111 @* -@ ;res spec
  112 @; test back
  113 @* -x jmp (-@)
  114 @*    jsr -x
  115 @* -@ ;redef spc
  116 @*    bcc -@
  117 @* -f iny ;redef
  118 @*    lda -f
  119 @*    eor -ah,x
  120 @* -d "jej"
  121 @; unresolved
  122 @*    jmp -u
  300 @; verify memory
  301 =1800 200918a9
  302 =*    0ba21890
  303 =*    02a1196c
  304 =*    0b18200b
  305 =*    1890fec8
  306 =*    ad131855
  307 =*    024a454a
  308 =*    4c0000
  999 print "{blue}[ok]{black}"
  1000 print "{blue}tests complete!"
  1005 new
