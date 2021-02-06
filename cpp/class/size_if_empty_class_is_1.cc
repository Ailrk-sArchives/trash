#include <cassert>
#include <iostream>

class Timer {};

int main(void) {

  Timer t{Timer()};
  assert(sizeof(t) == 1);
  std::cout << sizeof(t) << std::endl;
  return 0;
}
