@.str = private unnamed_addr constant [18 x i8] c"%d <-- factorial\0A\00", align 1

define i32 @factorial(i32 %n) {
entry:
  %i_initial = add i32 2, 0
  %f_initial = add i32 1, 0
  br label %check
check:
  %c_i = phi i32 [%i_initial, %entry], [%i_pp, %loop]
  %c_f = phi i32 [%f_initial, %entry], [%n_f, %loop]
  %i_leq = icmp sle i32 %c_i, %n
  br i1 %i_leq, label %loop, label %fin
loop:
  %n_f = mul i32 %c_f, %c_i
  %i_pp = add i32 %c_i, 1
  br label %check
fin:
  %r = phi i32 [%c_f, %check]
  ret i32 %r
}

define i32 @main(i32 %argc, i8** %argv) {
entry:
  %0 = call i32 @factorial(i32 3)
  %1 = mul i32 %0, 7
  %2 = icmp eq i32 %1, 42
  %3 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([18 x i8], [18 x i8]* @.str, i32 0, i32 0), i32 %0)
  %result = zext i1 %2 to i32
  ret i32 %result
}

declare i32 @printf(i8* nocapture readonly, ...);
