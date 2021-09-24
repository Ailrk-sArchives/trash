
int a = 10;
int b = 20;

static long s1 = 0xaaff;
constexpr long s2 = 0xffaa;

void push(int v) {
  asm("lea -4(%%ebp), %%ebp;"
      "movl %0, (%%ebp);"
      :
      : "r"(v)
      : "%ebx");
}


void pop(long &v) {
  asm("mov (%%ebp), %0;"
      "lea 4(%%ebp), %%ebp;"
      : "=r"(v));
}

inline void pop1(long &v) {
  asm("mov (%%ebp), %0;"
      "lea 4(%%ebp), %%ebp;"
      : "=r"(v));
}



extern "C" {
// turn off name mangling
void _start() {
  goto label;
lb:
  push(a);
  push(a);

label:
  pop(s1);
  pop1(s1);
  push(s1);
  goto lb;
}
}

// $ g++ -nostdlib t1.cc -m32 -fno-pie -O0 && objdump -d a.out
