   10 print"{up}  testing...     ";
  100 *-
  101 @1800 lda #"{clr}"
  102 @1802 ora #%11010110
  103 @*    ldx #$b2
  104 @*    ldy #100
  200 =1800 a99309d6a2b2a064
  999 print"{blue}[ok]{black}"
 1000 print "* symbolic assembly"
 1005 print "  {cyan}loading...{black}"
 1010 load"symboltest.prg",8

