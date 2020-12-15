#include "x86-simulator.h"


int main(void)
{
  Register r = regfile;

loop:
  cmp_reg_const(&r.rcx, 10);
  jge(endloop);
  std::cout << (r.rcx) << std::endl;
  inc(&r.rcx);

  jmp(loop);
endloop:

  return 0;
}
