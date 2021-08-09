#include "x86-simulator.h"

int main(void) {

_start:

  push('H');
  call(_push_char);

  push('e');
  call(_push_char);

  push('l');
  call(_push_char);

  push('l');
  call(_push_char);

  push('o');
  call(_push_char);

  push('\n');
  call(_push_char);

  jmp(_end);

_push_char:
  // prologue
  push(&r.rbp);
  move(&r.rbp, &r.rsp);
  sub_reg_const(&r.rsp, 8);
  push(&r.rdi);
  push(&r.rsi);

  // body
  move(&r.rax, Address{r.rbp + 8});
  printf("%c", r.rax);

  // epilogue
  pop(&r.rsi);
  pop(&r.rdi);
  move(&r.rsp, &r.rbp);
  pop(&r.rbp);
  ret;

_end:

  return 0;
}
