#include <iostream>
#include <memory>

struct Int {
  int i;
  Int(int i) : i(i) {}
};

int main(void) {
  auto a = std::make_unique<Int>(1);
  auto b = std::move(a);
  // now a {get(): 0x0}
  auto c = std::make_unique<Int>(*b);
  // now c is a unique ptr with copy of b.

  return 0;
}
