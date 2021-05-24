#include <concepts>
#include <iostream>

using namespace std::literals;

// selector: real -> *
// selector x = int ifx >= 42 else string
template <typename T, auto x>
concept selector =
    std::same_as<std::decay_t<T>,
                 std::conditional_t<(x >= 42), int, std::string>>;

// f: real[x] -> selector x -> ⊥
template <auto x> auto f(selector<x> auto) {}

int main(void) {
  f<42>(42);
  f<10>("opps"s);

  // f<64>("lol"s);
  return 0;
}
