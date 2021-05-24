#include <concepts>
#include <iostream>
#include <vector>

using namespace std::literals;

// class C a = { type G a, g : a -> G a }
//
// F: * -> *
// ∀ C a. F a = [G a]
// F _ = int
//
// f: ∀ a. a -> F a

auto f(auto x) {
  if constexpr (requires { x.g(); })
    return std::vector{x.g()};
  else
    return 42;
}

struct A {
  auto g() { return 2.81; }
};

struct B {
  auto g() { return "oops"s; }
};

struct C {};

int main(void) {
  auto x = f(A{});
  static_assert(std::same_as<decltype(x), std::vector<double>>);

  auto y = f(B{});
  static_assert(std::same_as<decltype(y), std::vector<std::string>>);

  auto z = f(C{});
  static_assert(std::same_as<decltype(z), int>);
  return 0;
}
