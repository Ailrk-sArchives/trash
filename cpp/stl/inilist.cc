#include <iostream>
#include <vector>

int main(void) {
  std::initializer_list<int> l{

#include "x.inc"

  };
  auto iter = l.begin();
  std::cout << sizeof(l) << std::endl;

  for (auto v : l) {
    std::cout << v << std::endl;
  }

  return 0;
}
