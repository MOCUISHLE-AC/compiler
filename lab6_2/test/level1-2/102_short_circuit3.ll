@a = global i32 0, align 4
@b = global i32 0, align 4
@d = global i32 0, align 4
define i32 @set_a(i32 %t3) {
B121:
  %t122 = alloca i32, align 4
  store i32 %t3, i32* %t122, align 4
  %t6 = load i32, i32* %t122, align 4
  store i32 %t6, i32* @a, align 4
  %t7 = load i32, i32* @a, align 4
  ret i32 %t7
}
define i32 @set_b(i32 %t8) {
B123:
  %t124 = alloca i32, align 4
  store i32 %t8, i32* %t124, align 4
  %t11 = load i32, i32* %t124, align 4
  store i32 %t11, i32* @b, align 4
  %t12 = load i32, i32* @b, align 4
  ret i32 %t12
}
define i32 @set_d(i32 %t13) {
B125:
  %t126 = alloca i32, align 4
  store i32 %t13, i32* %t126, align 4
  %t16 = load i32, i32* %t126, align 4
  store i32 %t16, i32* @d, align 4
  %t17 = load i32, i32* @d, align 4
  ret i32 %t17
}
define i32 @main() {
B127:
  %t213 = alloca i32, align 4
  %t212 = alloca i32, align 4
  %t211 = alloca i32, align 4
  %t210 = alloca i32, align 4
  %t209 = alloca i32, align 4
  %t146 = alloca i32, align 4
  store i32 2, i32* @a, align 4
  store i32 3, i32* @b, align 4
  %t20 = call i32 @set_a(i32 0)
  %t23 = icmp ne i32 %t20, 0
  br i1 %t23, label %B130, label %B132
B130:                               	; preds = %B127
  %t21 = call i32 @set_b(i32 1)
  %t24 = icmp ne i32 %t21, 0
  br i1 %t24, label %B128, label %B135
B132:                               	; preds = %B127
  br label %B129
B128:                               	; preds = %B130
  br label %B129
B135:                               	; preds = %B130
  br label %B129
B129:                               	; preds = %B128, %B132, %B135
  %t25 = load i32, i32* @a, align 4
  call void @putint(i32 %t25)
  call void @putch(i32 32)
  %t26 = load i32, i32* @b, align 4
  call void @putint(i32 %t26)
  call void @putch(i32 32)
  store i32 2, i32* @a, align 4
  store i32 3, i32* @b, align 4
  %t29 = call i32 @set_a(i32 0)
  %t32 = icmp ne i32 %t29, 0
  br i1 %t32, label %B139, label %B141
B139:                               	; preds = %B129
  %t30 = call i32 @set_b(i32 1)
  %t33 = icmp ne i32 %t30, 0
  br i1 %t33, label %B137, label %B144
B141:                               	; preds = %B129
  br label %B138
B137:                               	; preds = %B139
  br label %B138
B144:                               	; preds = %B139
  br label %B138
B138:                               	; preds = %B137, %B141, %B144
  %t34 = load i32, i32* @a, align 4
  call void @putint(i32 %t34)
  call void @putch(i32 32)
  %t35 = load i32, i32* @b, align 4
  call void @putint(i32 %t35)
  call void @putch(i32 10)
  store i32 1, i32* %t146, align 4
  store i32 2, i32* @d, align 4
  %t38 = load i32, i32* %t146, align 4
  %t39 = icmp sge i32 %t38, 1
  br i1 %t39, label %B149, label %B152
B149:                               	; preds = %B138
  %t40 = call i32 @set_d(i32 3)
  %t42 = icmp ne i32 %t40, 0
  br i1 %t42, label %B147, label %B154
B152:                               	; preds = %B138
  br label %B148
B147:                               	; preds = %B149
  br label %B148
B154:                               	; preds = %B149
  br label %B148
B148:                               	; preds = %B147, %B152, %B154
  %t43 = load i32, i32* @d, align 4
  call void @putint(i32 %t43)
  call void @putch(i32 32)
  %t44 = load i32, i32* %t146, align 4
  %t45 = icmp sle i32 %t44, 1
  br i1 %t45, label %B156, label %B161
B156:                               	; preds = %B148, %B158
  br label %B157
B161:                               	; preds = %B148
  br label %B158
B157:                               	; preds = %B156, %B163
  %t49 = load i32, i32* @d, align 4
  call void @putint(i32 %t49)
  call void @putch(i32 10)
  %t50 = add i32 2, 1
  %t51 = sub i32 3, %t50
  %t52 = icmp sge i32 16, %t51
  br i1 %t52, label %B165, label %B169
B158:                               	; preds = %B161
  %t46 = call i32 @set_d(i32 4)
  %t48 = icmp ne i32 %t46, 0
  br i1 %t48, label %B156, label %B163
B165:                               	; preds = %B157
  call void @putch(i32 65)
  br label %B166
B169:                               	; preds = %B157
  br label %B166
B163:                               	; preds = %B158
  br label %B157
B166:                               	; preds = %B165, %B169
  %t53 = sub i32 25, 7
  %t54 = mul i32 6, 3
  %t55 = sub i32 36, %t54
  %t56 = icmp ne i32 %t53, %t55
  br i1 %t56, label %B170, label %B174
B170:                               	; preds = %B166
  call void @putch(i32 66)
  br label %B171
B174:                               	; preds = %B166
  br label %B171
B171:                               	; preds = %B170, %B174
  %t57 = icmp slt i32 1, 8
  %t58 = srem i32 7, 2
  %t180 = zext i1 %t57 to i32
  %t59 = icmp ne i32 %t180, %t58
  br i1 %t59, label %B175, label %B183
B175:                               	; preds = %B171
  call void @putch(i32 67)
  br label %B176
B183:                               	; preds = %B171
  br label %B176
B176:                               	; preds = %B175, %B183
  %t60 = icmp sgt i32 3, 4
  %t189 = zext i1 %t60 to i32
  %t61 = icmp eq i32 %t189, 0
  br i1 %t61, label %B184, label %B192
B184:                               	; preds = %B176
  call void @putch(i32 68)
  br label %B185
B192:                               	; preds = %B176
  br label %B185
B185:                               	; preds = %B184, %B192
  %t62 = icmp sle i32 102, 63
  %t198 = zext i1 %t62 to i32
  %t63 = icmp eq i32 1, %t198
  br i1 %t63, label %B193, label %B201
B193:                               	; preds = %B185
  call void @putch(i32 69)
  br label %B194
B201:                               	; preds = %B185
  br label %B194
B194:                               	; preds = %B193, %B201
  %t64 = sub i32 5, 6
  %t204 = icmp ne i32 0, 0
  %t65 = xor i1 %t204, true
  %t205 = zext i1 %t65 to i32
  %t66 = sub i32 0, %t205
  %t67 = icmp eq i32 %t64, %t66
  br i1 %t67, label %B202, label %B208
B202:                               	; preds = %B194
  call void @putch(i32 70)
  br label %B203
B208:                               	; preds = %B194
  br label %B203
B203:                               	; preds = %B202, %B208
  call void @putch(i32 10)
  store i32 0, i32* %t209, align 4
  store i32 1, i32* %t210, align 4
  store i32 2, i32* %t211, align 4
  store i32 3, i32* %t212, align 4
  store i32 4, i32* %t213, align 4
  br label %B214
B214:                               	; preds = %B203, %B215
  %t73 = load i32, i32* %t209, align 4
  %t76 = icmp ne i32 %t73, 0
  br i1 %t76, label %B217, label %B219
B217:                               	; preds = %B214
  %t74 = load i32, i32* %t210, align 4
  %t77 = icmp ne i32 %t74, 0
  br i1 %t77, label %B215, label %B222
B219:                               	; preds = %B214
  br label %B216
B215:                               	; preds = %B217
  call void @putch(i32 32)
  br label %B214
B222:                               	; preds = %B217
  br label %B216
B216:                               	; preds = %B219, %B222
  %t78 = load i32, i32* %t209, align 4
  %t81 = icmp ne i32 %t78, 0
  br i1 %t81, label %B224, label %B228
B224:                               	; preds = %B216, %B226
  call void @putch(i32 67)
  br label %B225
B228:                               	; preds = %B216
  br label %B226
B225:                               	; preds = %B224, %B231
  %t83 = load i32, i32* %t209, align 4
  %t84 = load i32, i32* %t210, align 4
  %t85 = icmp sge i32 %t83, %t84
  br i1 %t85, label %B233, label %B238
B226:                               	; preds = %B228
  %t79 = load i32, i32* %t210, align 4
  %t82 = icmp ne i32 %t79, 0
  br i1 %t82, label %B224, label %B231
B233:                               	; preds = %B225, %B235
  call void @putch(i32 72)
  br label %B234
B238:                               	; preds = %B225
  br label %B235
B231:                               	; preds = %B226
  br label %B225
B234:                               	; preds = %B233, %B241
  %t90 = load i32, i32* %t211, align 4
  %t91 = load i32, i32* %t210, align 4
  %t92 = icmp sge i32 %t90, %t91
  br i1 %t92, label %B244, label %B247
B235:                               	; preds = %B238
  %t86 = load i32, i32* %t210, align 4
  %t87 = load i32, i32* %t209, align 4
  %t88 = icmp sle i32 %t86, %t87
  br i1 %t88, label %B233, label %B241
B244:                               	; preds = %B234
  %t93 = load i32, i32* %t213, align 4
  %t94 = load i32, i32* %t212, align 4
  %t95 = icmp ne i32 %t93, %t94
  br i1 %t95, label %B242, label %B250
B247:                               	; preds = %B234
  br label %B243
B241:                               	; preds = %B235
  br label %B234
B242:                               	; preds = %B244
  call void @putch(i32 73)
  br label %B243
B250:                               	; preds = %B244
  br label %B243
B243:                               	; preds = %B242, %B247, %B250
  %t97 = load i32, i32* %t209, align 4
  %t98 = load i32, i32* %t210, align 4
  %t255 = icmp ne i32 %t98, 0
  %t99 = xor i1 %t255, true
  %t256 = zext i1 %t99 to i32
  %t100 = icmp eq i32 %t97, %t256
  br i1 %t100, label %B254, label %B259
B254:                               	; preds = %B243
  %t101 = load i32, i32* %t212, align 4
  %t102 = load i32, i32* %t212, align 4
  %t103 = icmp slt i32 %t101, %t102
  br i1 %t103, label %B251, label %B262
B259:                               	; preds = %B243
  br label %B253
B251:                               	; preds = %B253, %B254
  call void @putch(i32 74)
  br label %B252
B262:                               	; preds = %B254
  br label %B253
B253:                               	; preds = %B259, %B262
  %t105 = load i32, i32* %t213, align 4
  %t106 = load i32, i32* %t213, align 4
  %t107 = icmp sge i32 %t105, %t106
  br i1 %t107, label %B251, label %B265
B252:                               	; preds = %B251, %B265
  %t109 = load i32, i32* %t209, align 4
  %t110 = load i32, i32* %t210, align 4
  %t269 = icmp ne i32 %t110, 0
  %t111 = xor i1 %t269, true
  %t270 = zext i1 %t111 to i32
  %t112 = icmp eq i32 %t109, %t270
  br i1 %t112, label %B266, label %B273
B265:                               	; preds = %B253
  br label %B252
B266:                               	; preds = %B252, %B274
  call void @putch(i32 75)
  br label %B267
B273:                               	; preds = %B252
  br label %B268
B267:                               	; preds = %B266, %B277, %B280
  call void @putch(i32 10)
  ret i32 0
B268:                               	; preds = %B273
  %t113 = load i32, i32* %t212, align 4
  %t114 = load i32, i32* %t212, align 4
  %t115 = icmp slt i32 %t113, %t114
  br i1 %t115, label %B274, label %B277
B274:                               	; preds = %B268
  %t116 = load i32, i32* %t213, align 4
  %t117 = load i32, i32* %t213, align 4
  %t118 = icmp sge i32 %t116, %t117
  br i1 %t118, label %B266, label %B280
B277:                               	; preds = %B268
  br label %B267
B280:                               	; preds = %B274
  br label %B267
}
declare void @putint(i32)
declare void @putch(i32)
