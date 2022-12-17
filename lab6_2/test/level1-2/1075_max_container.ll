define i32 @maxArea(i32* %t0, i32 %t2) {
B71:
  %t83 = alloca i32, align 4
  %t76 = alloca i32, align 4
  %t75 = alloca i32, align 4
  %t74 = alloca i32, align 4
  %t73 = alloca i32, align 4
  %t72 = alloca i32*, align 4
  store i32* %t0, i32** %t72, align 4
  store i32 %t2, i32* %t73, align 4
  store i32 0, i32* %t74, align 4
  %t8 = load i32, i32* %t73, align 4
  %t9 = sub i32 %t8, 1
  store i32 %t9, i32* %t75, align 4
  %t12 = sub i32 0, 1
  store i32 %t12, i32* %t76, align 4
  br label %B77
B77:                               	; preds = %B71, %B97
  %t13 = load i32, i32* %t74, align 4
  %t14 = load i32, i32* %t75, align 4
  %t15 = icmp slt i32 %t13, %t14
  br i1 %t15, label %B78, label %B82
B78:                               	; preds = %B77
  %t21 = icmp slt i32* %t18, %t20
  br i1 %t21, label %B84, label %B89
B82:                               	; preds = %B77
  br label %B79
B84:                               	; preds = %B78
  %t23 = load i32, i32* %t75, align 4
  %t24 = load i32, i32* %t74, align 4
  %t25 = sub i32 %t23, %t24
  %t28 = mul i32 %t25, %t27
  store i32 %t28, i32* %t83, align 4
  br label %B86
B89:                               	; preds = %B78
  br label %B85
B79:                               	; preds = %B82
  %t52 = load i32, i32* %t76, align 4
  ret i32 %t52
B86:                               	; preds = %B84, %B85
  %t36 = load i32, i32* %t83, align 4
  %t37 = load i32, i32* %t76, align 4
  %t38 = icmp sgt i32 %t36, %t37
  br i1 %t38, label %B90, label %B94
B85:                               	; preds = %B89
  %t30 = load i32, i32* %t75, align 4
  %t31 = load i32, i32* %t74, align 4
  %t32 = sub i32 %t30, %t31
  %t35 = mul i32 %t32, %t34
  store i32 %t35, i32* %t83, align 4
  br label %B86
B90:                               	; preds = %B86
  %t40 = load i32, i32* %t83, align 4
  store i32 %t40, i32* %t76, align 4
  br label %B91
B94:                               	; preds = %B86
  br label %B91
B91:                               	; preds = %B90, %B94
  %t45 = icmp sgt i32* %t42, %t44
  br i1 %t45, label %B95, label %B100
B95:                               	; preds = %B91
  %t47 = load i32, i32* %t75, align 4
  %t48 = sub i32 %t47, 1
  store i32 %t48, i32* %t75, align 4
  br label %B97
B100:                               	; preds = %B91
  br label %B96
B97:                               	; preds = %B95, %B96
  br label %B77
B96:                               	; preds = %B100
  %t50 = load i32, i32* %t74, align 4
  %t51 = add i32 %t50, 1
  store i32 %t51, i32* %t74, align 4
  br label %B97
}
define i32 @main() {
B101:
  %t103 = alloca [10 x i32], align 4
  %t102 = alloca i32, align 4
  store i32 3, i32* %t55, align 4
  store i32 3, i32* %t56, align 4
  store i32 9, i32* %t57, align 4
  store i32 0, i32* %t58, align 4
  store i32 0, i32* %t59, align 4
  store i32 1, i32* %t60, align 4
  store i32 1, i32* %t61, align 4
  store i32 5, i32* %t62, align 4
  store i32 7, i32* %t63, align 4
  store i32 8, i32* %t64, align 4
  store i32 10, i32* %t102, align 4
  %t68 = load i32, i32* %t102, align 4
  %t69 = call i32 @maxArea(i32* %t67, i32 %t68)
  store i32 %t69, i32* %t102, align 4
  %t70 = load i32, i32* %t102, align 4
  ret i32 %t70
}
