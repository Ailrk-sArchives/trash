#include <iostream>

struct S {
  const int &m;

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

// initializa const int& with a
// temporary S{1}, then assign that to the const
// reference.
[[nodiscard]] int foo(void) {
  const S &s = S{1};
  return s.m;
}

int main(void) {
  auto v = foo();
  std::cout << v << std::endl;
  return 0;
}
