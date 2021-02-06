#include <iostream>

struct S {
  S() { puts("S()"); }
  S(int) { puts("S(int)"); }
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

struct Holder {
  S s;
  int i;
};

Holder get_holder() { return {}; }

S get_s() {
  // Note it's a move here.
  // get_holder return a rvalue.
  // and then you are accessing the member s
  // of the r value. Compiler know it, and see
  // it as a perfect candidate for calling a move constructor
  S s = get_holder().s;
  return s;
}

int main(void) {
  S s = get_s();
  return 0;
}
