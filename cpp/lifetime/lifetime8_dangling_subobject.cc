#include <iostream>
#include <vector>

struct S {
  std::vector<int> data{1, 2, 3, 4, 5};
  const auto &get_data() const { return data; }
};

S get_s() { return S{}; }

// this is an UB!
int main(void) {
  for (const auto &v : get_s().get_data()) {
    std::cout << v;
  }

  return 0;
}

// desugared version of the ranged for loop.
void main__() {
  // not recursive object lifetime extension.
  auto &&range__ = get_s().get_data();
  auto begin__ = std::begin(range__);
  auto end__ = std::end(range__);
  for (; begin__ != end__; ++begin__) {
    const auto &v = *begin__;
    std::cout << v;
  }
}
