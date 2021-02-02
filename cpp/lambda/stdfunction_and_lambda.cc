#include <functional>
#include <iostream>

int main(void) {

  // this will segfault !
  // the return value is a reference to 42,
  // but after invocation 42 is gonne.
  std::function<const int &()> f([] { return 42; });
  int x = f();

  return 0;
}
