;; define a simple module.
;; a module is a list of global values (both variables and function are llvm::Value.
;; global values are represented as a pointer to a memory location.

;; linkage type:
;;  global values and functions can have their linkage types.
;;  private/weak/internal/common/external/...

;; calling convention:
;; LLVM ir is not tighted to any pariticular calling convention. you can choose
;; the calling convention you like.
;;   ccc: c calling convention.
;;   fastcc: fast calling convention.
;;   cold: cold calling convention.
;;   cc 10: ghc calling convention...
;;    ...


; struct
%mytype = type { %mytype*, i32 }

; decalre the string constant as global variable
@.str = private unnamed_addr constant [13 x i8] c"hello world\0A\00"

; external declaration of puts function
declare i32 @puts(i8* nocapture) nounwind

; define main function
define i32 @main() {

  ; convert [i3 x i8]* to i8*
  %case210 = getelementptr [13 x i8], [13 x i8]* @.str, i64 0, i64 0

  ; note all function call needs to mark the parameter and return type as well.
  ; since it's IR, we don't really care about ergnomics. The more information
  ; the better.
  call i32 @puts(i8* %case210)
  ret i32 0
}

; named metadata
!0 = !{i32 42, null, !"string"}
!foo = !{!0}
