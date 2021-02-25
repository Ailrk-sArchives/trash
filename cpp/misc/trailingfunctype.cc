#include <iostream>

// it's just for a function pointer with return type void;
template <typename F> using FnPtr = auto (*)(F) -> int;

// function pointer wrapper for all function types.
template <typename R, typename... Args> using FPtr = auto (*)(Args...) -> R;

int foo(int a) {
  std::cout << "good" << std::endl;
  return 1 + a;
}

using T1 = FnPtr<int>;

void bar(FnPtr<int> ptr) {}

int main(void) {
  bar(foo);
  return 0;
}
