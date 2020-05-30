10 print "wax assembly test"
15 print "* all instructions"
20 print "* all addressing modes* time: 14 seconds
25 input "press return to start";a$
30 print "testing..."
100 @1c00 adc #$00
101 @1c02 adc $10
102 @1c04 adc $20,x
103 @1c06 adc $3030
104 @1c09 adc $4040,x
105 @1c0c adc $5050,y
106 @1c0f adc ($60,x)
107 @1c11 adc ($70),y
108 @1c13 and #$80
109 @1c15 and $90
110 @1c17 and $10,x
111 @1c19 and $1111
112 @1c1c and $1212,x
113 @1c1f and $1313,y
114 @1c22 and ($14,x)
115 @1c24 and ($15),y
116 @1c26 asl 
117 @1c27 asl $17
118 @1c29 asl $18,x
119 @1c2b asl $1919
120 @1c2e asl $2020,x
121 @1c31 bcc $1c31
122 @1c32 bcs $1c32
123 @1c33 beq $1c33
124 @1c34 bit $24
125 @1c36 bit $2525
126 @1c39 bmi $1c39
127 @1c3a bne $1c3a
128 @1c3b bpl $1c3b
129 @1c3c brk 
130 @1c3d bvc $1c3d
131 @1c3e bvs $1c3e
132 @1c3f clc 
133 @1c40 cld 
134 @1c41 cli 
135 @1c42 clv 
136 @1c43 cmp #$36
137 @1c45 cmp $37
138 @1c47 cmp $38,x
139 @1c49 cmp $3939
140 @1c4c cmp $4040,x
141 @1c4f cmp $4141,y
142 @1c52 cmp ($42,x)
143 @1c54 cmp ($43),y
144 @1c56 cpx #$44
145 @1c58 cpx $45
146 @1c5a cpx $4646
147 @1c5d cpy #$47
148 @1c5f cpy $48
149 @1c61 cpy $4949
150 @1c64 dec $50
151 @1c66 dec $51,x
152 @1c68 dec $5252
153 @1c6b dec $5353,x
154 @1c6e dex 
155 @1c6f dey 
156 @1c70 eor #$56
157 @1c72 eor $57
158 @1c74 eor $58,x
159 @1c76 eor $5959
160 @1c79 eor $6060,x
161 @1c7c eor $6161,y
162 @1c7f eor ($62,x)
163 @1c81 eor ($63),y
164 @1c83 inc $64
165 @1c85 inc $65,x
166 @1c87 inc $6666
167 @1c8a inc $6767,x
168 @1c8d inx 
169 @1c8e iny 
170 @1c8f jmp $7070
171 @1c92 jmp ($7171)
172 @1c95 jsr $7272
173 @1c98 lda #$73
174 @1c9a lda $74
175 @1c9c lda $75,x
176 @1c9e lda $7676
177 @1ca1 lda $7777,x
178 @1ca4 lda $7878,y
179 @1ca7 lda ($79,x)
180 @1ca9 lda ($80),y
181 @1cab ldx #$81
182 @1cad ldx $82
183 @1caf ldx $83,y
184 @1cb1 ldx $8484
185 @1cb4 ldx $8585,y
186 @1cb7 ldy #$86
187 @1cb9 ldy $87
188 @1cbb ldy $88,x
189 @1cbd ldy $8989
190 @1cc0 ldy $9090,x
191 @1cc3 lsr 
192 @1cc4 lsr $92
193 @1cc6 lsr $93,x
194 @1cc8 lsr $9494
195 @1ccb lsr $9595,x
196 @1cce nop 
197 @1ccf ora #$97
198 @1cd1 ora $98
199 @1cd3 ora $99,x
200 @1cd5 ora $0000
201 @1cd8 ora $1010,x
202 @1cdb ora $2020,y
203 @1cde ora ($30,x)
204 @1ce0 ora ($40),y
205 @1ce2 pha 
206 @1ce3 php 
207 @1ce4 pla 
208 @1ce5 plp 
209 @1ce6 rol 
210 @1ce7 rol $10
211 @1ce9 rol $11,x
212 @1ceb rol $1212
213 @1cee rol $1313,x
214 @1cf1 ror 
215 @1cf2 ror $15
216 @1cf4 ror $16,x
217 @1cf6 ror $1717
218 @1cf9 ror $1818,x
219 @1cfc rti 
220 @1cfd rts 
221 @1cfe sbc #$21
222 @1d00 sbc $22
223 @1d02 sbc $23,x
224 @1d04 sbc $2424
225 @1d07 sbc $2525,x
226 @1d0a sbc $2626,y
227 @1d0d sbc ($27,x)
228 @1d0f sbc ($28),y
229 @1d11 sec 
230 @1d12 sed 
231 @1d13 sei 
232 @1d14 sta $32
233 @1d16 sta $33,x
234 @1d18 sta $3434
235 @1d1b sta $3535,x
236 @1d1e sta $3636,y
237 @1d21 sta ($37,x)
238 @1d23 sta ($38),y
239 @1d25 stx $39
240 @1d27 stx $40,y
241 @1d29 stx $4141
242 @1d2c sty $42
243 @1d2e sty $43,x
244 @1d30 sty $4444
245 @1d33 tax 
246 @1d34 tay 
247 @1d35 tsx 
248 @1d36 txa 
249 @1d37 txs 
250 @1d38 tya 
999 print "success!"
