#include "x86-simulator.h"

int main(void)
{

  Register r = regfile;
_start:

_push_char:   // subroutine
  push(&r.rbp);
  move(&r.rbp, &r.rsp);

  return 0;
}
