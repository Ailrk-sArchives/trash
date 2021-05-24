// parametric polymorphism

#include <concepts>
#include <iostream>

using namespace std::literals;

auto id(auto x) { return x; }

int main(void) {
  auto x = id(13);
  auto y = id(13.1);
  auto z = id("asd"s);

  static_assert(std::same_as<decltype(x), int>);
  static_assert(std::same_as<decltype(y), double>);
  static_assert(std::same_as<decltype(z), std::string>);
  return 0;
}
