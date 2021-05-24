#include <functional>
#include <iostream>
#include <vector>

// - Bounded quantification: Basically type constraint.
//   (Interaction of parametric polymorphism with subtyping)
//
// https://en.wikipedia.org/wiki/Bounded_quantification

// type E = ∃ a. x: a, print: a -> string -> ⊥
struct E {
  using QuantificationBound = auto(std::string_view) -> void;
  std::function<QuantificationBound> f{};

  E() = default;

  // exists an x, doesn't mater what type it is.
  E(auto x) {
    // we can print with x, but nothing else.
    f = [=](auto msg) { x.print(msg); };
  }

  auto print(auto msg) const { f(msg); }
};

struct A {
  auto print(auto msg) const { std::cout << "A: " << msg << std::endl; }
};

struct B {
  auto print(auto msg) const { std::cout << "B: " << msg << std::endl; }
};

// achieve a similar effect as subtype polymorphism.
auto main() -> int {
  auto xs = std::vector<E>{A{}, B{}};
  for (auto &x : xs) {
    x.print("hi");
  }
}
