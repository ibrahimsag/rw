@.str = private unnamed_addr constant [18 x i8] c"%d <-- fibonacci\0A\00", align 1

%ab = type {i32, i32}

define i32 @fibonacci(i32 %n) {
entry:
  %p = alloca %ab
  %p_a = getelementptr %ab, %ab* %p, i32 0, i32 0
  store i32 1, i32* %p_a
  %p_b = getelementptr %ab, %ab* %p, i32 0, i32 1
  store i32 1, i32* %p_b

  %i_init = add i32 1, 0
  br label %check
check:
  %c_i = phi i32 [%i_init, %entry], [%i_pp, %loop]
  %i_leq = icmp sle i32 %c_i, %n
  br i1 %i_leq, label %loop, label %fin
loop:
  %c_a = load i32, i32* %p_a
  %c_b = load i32, i32* %p_b
  %n_b = add i32 %c_a, %c_b
  store i32 %c_b, i32* %p_a
  store i32 %n_b, i32* %p_b

  %i_pp = add i32 %c_i, 1
  br label %check
fin:
  %r = load i32, i32* %p_b
  ret i32 %r
}

define i32 @main(i32 %argc, i8** %argv) {
entry:
  %0 = call i32 @fibonacci(i32 5)
  %1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([18 x i8], [18 x i8]* @.str, i32 0, i32 0), i32 %0)
  ret i32 0
}

declare i32 @printf(i8* nocapture readonly, ...);
