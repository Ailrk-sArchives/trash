#include <iostream>

struct S {
  int i;
  constexpr S(int i) : i(i) {}
};


int main(void)
{
  constexpr S s(1);

  return 0;
}
