#include <iostream>
#include <ranges>
#include <vector>

void vec_range_test() {
  std::vector<int> numbers{1, 2, 3, 4};
  auto results = numbers

                 | std::views::filter([](int n) { return n % 2 == 0; }) u

                 | std::views::transform([](int n) { return n * 2; })

                 | std::views::transform([](int n) { return 'c'; });

  for (auto v : results) {
    std::cout << v << ", " << std::endl;
  }
}

int main(void) {
  vec_range_test();
  return 0;
}
