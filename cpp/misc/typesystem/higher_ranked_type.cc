#include <functional>
#include <iostream>
#include <tuple>

using namespace std::literals;

// p: (∀ a. a -> a) -> ∀ ...b. b... -> π b...

auto p(auto f, auto... x) { return std::make_tuple(f(x)...); }

auto main(void) -> int {
  auto [x, y, z] = p([](auto x) { return x + x; }, 21, 1.5, "ja"s);

  return 0;
}
