#include <iostream>

struct S {
  S() { puts("S()"); }
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

  // some c++ attributes
  [[deprecated]] int foo() { return 1; }
  [[noreturn, deprecated]] void bar() { exit(1); }
  [[nodiscard]] int flag(int a) {
    if (a > 10) {
      return 1;
    } else {
      return -1;
    }
  }
};

// Nothing get constructed in the scope.
// Reference type is not an object type, so in this case
// it doesn't affect the lifetime of the obejct.
void f1() {
  S s; // S()
  { [[maybe_unused]] S &s2{s}; }
  // ~S()
}

// s2 copy constructed from s.
void f2() {
  S s; // S()
  {
    S s2{s}; // S(const &S)
  }          // ~S()
  // ~S()
}

int main(void) {
  f1();
  puts("");
  f2();

  return 0;
}
