#include <iostream>

struct Foo {
  int i;
  float f;
  char c;
};

// pretty cool you can specify your own
// alignment.
struct alignas(64) Foo64 {
  int i;
  float x;
};

int main(void) {

  std::cout << "Alignment of "
            << "char       :" << alignof(char)
            << "\n"
               "poiter     :"
            << alignof(int *)
            << "\n"
               "class Foo  :"
            << alignof(Foo) << "\n"
            << "class Foo64 :" << alignof(Foo64) << "\n"
            << std::endl;

  return 0;
}
