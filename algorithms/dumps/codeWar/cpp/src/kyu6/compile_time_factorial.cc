#include <iostream>

template <int x> struct factorial {
  const static unsigned long long value = x * factorial<x-1>::value;
};
template <> struct factorial<0> { const static unsigned long long value = 1; };


int main(void)
{
  unsigned long long a = factorial<20>::value;
  std::cout << a << std::endl;
  return 0;
}
