#include <iostream>

// Sub objects 1

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

// be optimzied off with return value optimization.
S get_s() { return {}; }
S get_another_s() { return {}; }

// it will be optmized off with Named return value optimization
S get_another_ss() {
  S s;
  return s;
}

int main(void) {
  // get_s is as if deleted.
  // S is constructed right here.
  S s = get_another_ss();
  return 0;
}
