;; define the format string
@format_string = private unnamed_addr constant [3 x i8] c"%d\00", align 1
declare i32 @printf(i8*, ...)

define i32 @fib_helper(i32 %nprev1, i32 %nprev2, i32 %n) {
  %1 = icmp eq i32 %n, 1

  br i1 %1, label %.base, label %.inductive

.base:
  ret i32 %nprev2

.inductive:
  %2 = add i32 %nprev2, %nprev1
  %3 = sub i32 %n, 1
  %4 = tail call i32 @fib_helper(i32 %nprev2, i32 %2, i32 %3)
  ret i32 %4
}

define i32 @fib(i32 %n) {
  %1 = tail call i32 @fib_helper(i32 0, i32 1, i32 %n)
  ret i32 %1
}

define i32 @main() {
  %1 = tail call i32 @fib(i32 12)
  ; create a format pointer
  %format_ptr = getelementptr inbounds [3 x i8], [3 x i8]* @format_string, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %format_ptr, i32 %1)
  ret i32 0
}
