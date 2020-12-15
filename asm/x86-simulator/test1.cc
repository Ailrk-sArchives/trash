#include "x86-simulator.h"


int main(void)
{
  Register r = regfile;

loop:
  I_cmp_reg_const(&r.ecx, 10);
  jge(endloop);
  std::cout << (r.ecx) << std::endl;
  I_inc(&r.ecx);

  jmp(loop);
endloop:

  return 0;
}
