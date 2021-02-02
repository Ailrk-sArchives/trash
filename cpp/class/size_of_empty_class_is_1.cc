#include <iostream>

class Timer {};

int main(void) {

  Timer t{Timer()};
  std::cout << sizeof(t) << std::endl;
  return 0;
}
