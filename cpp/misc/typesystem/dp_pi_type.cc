
#include <concepts>
#include <iostream>
#include <vector>

using namespace std::literals;

// type of auto depend on the value of x.
// of course in C++ this only works at compile time.
template <auto x> auto f() {
  if constexpr (x >= 42)
    return 42;
  else
    return "oops"s;
}

int main(void) {
  auto x = f<10>();
  auto y = f<42>();

  static_assert(std::same_as<decltype(x), std::string>);
  static_assert(std::same_as<decltype(y), int>);
}
