#include <iostream>

struct S {
  S() { puts("S()"); }
  S(int) {
    // exception thrown here.
    // the constructor is not completed.
    // So the lifetime of the object is not started.
    // This obejct is not created what so ever.
    throw 1;
    puts("S(int)");
  }
  S(const S &) noexcept { puts("S(const S&)"); }
  S(S &&) noexcept { puts("S(S &&)"); }
  S &operator=(const S &) {
    puts("operator=(const S&)");
    return *this;
  }

  S &operator=(S &&) {
    puts("S &operator=S &&()");
    return *this;
  }

  ~S() { puts("~S()"); }
};

int main(void) {

  try {
    S s{1};

  } catch (...) {
  }
  return 0;
}
