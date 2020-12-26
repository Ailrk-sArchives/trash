#include <iostream>
#include <optional>

template <typename T> int foo(std::optional<T> v) { return v.value_or(-1); }

int main(void) {
  auto x = std::optional<int>{1};

  std::cout << foo<int>({}) << std::endl;
  std::cout << foo<int>({1}) << std::endl;

  return 0;
}
