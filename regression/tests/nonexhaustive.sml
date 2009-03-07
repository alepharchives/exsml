(* nonexhaustive.sml *)

val _ =
   case 2 of
      2 => 3
    | 3 => 4
         
val _ =
   case [] of
      [] => 1

val _ =
   case [] of
      [] => 0
    | [_] => 1
    | [_, _] => 2

fun first l =
   case l of
      SOME x :: _ => x

fun f x =
   case x of
      (false, false) => ()
    | (true, true) => ()

datatype z = A | B | C

fun f x =
   case x of
      (A, B, C) => ()

val _ =
   case Fail "foo" of
      Fail _ => false

val _ =
   case (1, []) of
      (1, []) => true

val _ =
   case (1, []) of
      (1, _) => true

fun f 1 2 = 3

fun f "" = ()

fun f #"a" = 13
   
fun f (0w0: Word8.word) = 13

(* Checks for non-exhaustive pattern matches (compiler should warn). *)

fun ord #"\000" = 0
  | ord #"\002" = 2
  | ord #"\003" = 3
  | ord #"\004" = 4
  | ord #"\005" = 5
  | ord #"\006" = 6
  | ord #"\007" = 7
  | ord #"\008" = 8
  | ord #"\009" = 9
  | ord #"\010" = 10
  | ord #"\011" = 11
  | ord #"\012" = 12
  | ord #"\013" = 13
  | ord #"\014" = 14
  | ord #"\015" = 15
  | ord #"\016" = 16
  | ord #"\017" = 17
  | ord #"\018" = 18
  | ord #"\019" = 19
  | ord #"\020" = 20
  | ord #"\021" = 21
  | ord #"\022" = 22
  | ord #"\023" = 23
  | ord #"\024" = 24
  | ord #"\025" = 25
  | ord #"\026" = 26
  | ord #"\027" = 27
  | ord #"\028" = 28
  | ord #"\029" = 29
  | ord #"\030" = 30
  | ord #"\031" = 31
  | ord #"\032" = 32
  | ord #"\033" = 33
  | ord #"\034" = 34
  | ord #"\035" = 35
  | ord #"\036" = 36
  | ord #"\037" = 37
  | ord #"\038" = 38
  | ord #"\039" = 39
  | ord #"\040" = 40
  | ord #"\041" = 41
  | ord #"\042" = 42
  | ord #"\043" = 43
  | ord #"\044" = 44
  | ord #"\045" = 45
  | ord #"\046" = 46
  | ord #"\047" = 47
  | ord #"\048" = 48
  | ord #"\049" = 49
  | ord #"\050" = 50
  | ord #"\051" = 51
  | ord #"\052" = 52
  | ord #"\053" = 53
  | ord #"\054" = 54
  | ord #"\055" = 55
  | ord #"\056" = 56
  | ord #"\057" = 57
  | ord #"\058" = 58
  | ord #"\059" = 59
  | ord #"\060" = 60
  | ord #"\061" = 61
  | ord #"\062" = 62
  | ord #"\063" = 63
  | ord #"\064" = 64
  | ord #"\065" = 65
  | ord #"\066" = 66
  | ord #"\067" = 67
  | ord #"\068" = 68
  | ord #"\069" = 69
  | ord #"\070" = 70
  | ord #"\071" = 71
  | ord #"\072" = 72
  | ord #"\073" = 73
  | ord #"\074" = 74
  | ord #"\075" = 75
  | ord #"\076" = 76
  | ord #"\077" = 77
  | ord #"\078" = 78
  | ord #"\079" = 79
  | ord #"\080" = 80
  | ord #"\081" = 81
  | ord #"\082" = 82
  | ord #"\083" = 83
  | ord #"\084" = 84
  | ord #"\085" = 85
  | ord #"\086" = 86
  | ord #"\087" = 87
  | ord #"\088" = 88
  | ord #"\089" = 89
  | ord #"\090" = 90
  | ord #"\091" = 91
  | ord #"\092" = 92
  | ord #"\093" = 93
  | ord #"\094" = 94
  | ord #"\095" = 95
  | ord #"\096" = 96
  | ord #"\097" = 97
  | ord #"\098" = 98
  | ord #"\099" = 99
  | ord #"\100" = 100
  | ord #"\101" = 101
  | ord #"\102" = 102
  | ord #"\103" = 103
  | ord #"\104" = 104
  | ord #"\105" = 105
  | ord #"\106" = 106
  | ord #"\107" = 107
  | ord #"\108" = 108
  | ord #"\109" = 109
  | ord #"\110" = 110
  | ord #"\111" = 111
  | ord #"\112" = 112
  | ord #"\113" = 113
  | ord #"\114" = 114
  | ord #"\115" = 115
  | ord #"\116" = 116
  | ord #"\117" = 117
  | ord #"\118" = 118
  | ord #"\119" = 119
  | ord #"\120" = 120
  | ord #"\121" = 121
  | ord #"\122" = 122
  | ord #"\123" = 123
  | ord #"\124" = 124
  | ord #"\125" = 125
  | ord #"\126" = 126
  | ord #"\127" = 127
  | ord #"\128" = 128
  | ord #"\129" = 129
  | ord #"\130" = 130
  | ord #"\131" = 131
  | ord #"\132" = 132
  | ord #"\133" = 133
  | ord #"\134" = 134
  | ord #"\135" = 135
  | ord #"\136" = 136
  | ord #"\137" = 137
  | ord #"\138" = 138
  | ord #"\139" = 139
  | ord #"\140" = 140
  | ord #"\141" = 141
  | ord #"\142" = 142
  | ord #"\143" = 143
  | ord #"\144" = 144
  | ord #"\145" = 145
  | ord #"\146" = 146
  | ord #"\147" = 147
  | ord #"\148" = 148
  | ord #"\149" = 149
  | ord #"\150" = 150
  | ord #"\151" = 151
  | ord #"\152" = 152
  | ord #"\153" = 153
  | ord #"\154" = 154
  | ord #"\155" = 155
  | ord #"\156" = 156
  | ord #"\157" = 157
  | ord #"\158" = 158
  | ord #"\159" = 159
  | ord #"\160" = 160
  | ord #"\161" = 161
  | ord #"\162" = 162
  | ord #"\163" = 163
  | ord #"\164" = 164
  | ord #"\165" = 165
  | ord #"\166" = 166
  | ord #"\167" = 167
  | ord #"\168" = 168
  | ord #"\169" = 169
  | ord #"\170" = 170
  | ord #"\171" = 171
  | ord #"\172" = 172
  | ord #"\173" = 173
  | ord #"\174" = 174
  | ord #"\175" = 175
  | ord #"\176" = 176
  | ord #"\177" = 177
  | ord #"\178" = 178
  | ord #"\179" = 179
  | ord #"\180" = 180
  | ord #"\181" = 181
  | ord #"\182" = 182
  | ord #"\183" = 183
  | ord #"\184" = 184
  | ord #"\185" = 185
  | ord #"\186" = 186
  | ord #"\187" = 187
  | ord #"\188" = 188
  | ord #"\189" = 189
  | ord #"\190" = 190
  | ord #"\191" = 191
  | ord #"\192" = 192
  | ord #"\193" = 193
  | ord #"\194" = 194
  | ord #"\195" = 195
  | ord #"\196" = 196
  | ord #"\197" = 197
  | ord #"\198" = 198
  | ord #"\199" = 199
  | ord #"\200" = 200
  | ord #"\201" = 201
  | ord #"\202" = 202
  | ord #"\203" = 203
  | ord #"\204" = 204
  | ord #"\205" = 205
  | ord #"\206" = 206
  | ord #"\207" = 207
  | ord #"\208" = 208
  | ord #"\209" = 209
  | ord #"\210" = 210
  | ord #"\211" = 211
  | ord #"\212" = 212
  | ord #"\213" = 213
  | ord #"\214" = 214
  | ord #"\215" = 215
  | ord #"\216" = 216
  | ord #"\217" = 217
  | ord #"\218" = 218
  | ord #"\219" = 219
  | ord #"\220" = 220
  | ord #"\221" = 221
  | ord #"\222" = 222
  | ord #"\223" = 223
  | ord #"\224" = 224
  | ord #"\225" = 225
  | ord #"\226" = 226
  | ord #"\227" = 227
  | ord #"\228" = 228
  | ord #"\229" = 229
  | ord #"\230" = 230
  | ord #"\231" = 231
  | ord #"\232" = 232
  | ord #"\233" = 233
  | ord #"\234" = 234
  | ord #"\235" = 235
  | ord #"\236" = 236
  | ord #"\237" = 237
  | ord #"\238" = 238
  | ord #"\239" = 239
  | ord #"\240" = 240
  | ord #"\241" = 241
  | ord #"\242" = 242
  | ord #"\243" = 243
  | ord #"\244" = 244
  | ord #"\245" = 245
  | ord #"\246" = 246
  | ord #"\247" = 247
  | ord #"\248" = 248
  | ord #"\249" = 249
  | ord #"\250" = 250
  | ord #"\251" = 251
  | ord #"\252" = 252
  | ord #"\253" = 253
  | ord #"\254" = 254
  | ord #"\255" = 255;
