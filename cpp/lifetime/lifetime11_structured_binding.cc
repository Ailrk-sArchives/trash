#include <iostream>

// Strucutured bindings create hidden values that are
// refrences (aren't object),
// RVO and automatic move cannot happen witha reference

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

// use structured binding.
S get_s() {
  // call a copy constructor instead.
  auto [s, i] = get_holder();
  return s;
}

// what's really going on here
S get_S_desugared() {
  auto e = get_holder();
  auto &s = e.s;
  auto &i = e.i;
  return s; // RVO doesn't apply for a reference.
}

int main(void) {
  S s = get_s();

  return 0;
}
