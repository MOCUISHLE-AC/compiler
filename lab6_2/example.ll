define i32 @main() {
B8:
  %t10 = alloca i32, align 4
  %t9 = alloca i32, align 4
  store i32 1, i32* %t9, align 4
  store i32 2, i32* %t10, align 4
  store float32 1, float32* %t11, align 4
  %t3 = load i32, i32* %t9, align 4
  %t4 = load i32, i32* %t10, align 4
  %t5 = icmp sle i32 %t3, %t4
  br i1 %t5, label %B12, label %B17
B12:                               	; preds = %B8
  store i32 1, i32* %t9, align 4
  br label %B14
B17:                               	; preds = %B8
  br label %B13
B14:                               	; preds = %B12, %B13
  ret i32 0
B13:                               	; preds = %B17
  store i32 2, i32* %t9, align 4
  br label %B14
}
