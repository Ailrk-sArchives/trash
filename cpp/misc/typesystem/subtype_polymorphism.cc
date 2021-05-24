#include <iostream>
#include <memory>
#include <vector>

// just normal virtual functions

struct E {
  virtual auto print(std::string_view msg) -> void = 0;
};

// A <: E
struct A : E {
  auto print(std::string_view msg) -> void override {
    std::cout << "A: " << msg << std::endl;
  }
};

// B <: E
struct B : E {
  auto print(std::string_view msg) -> void override {
    std::cout << "B: " << msg << std::endl;
  }
};

int main(void) {
  auto xs = std::vector<std::unique_ptr<E>>{};
  xs.emplace_back(std::make_unique<A>());
  xs.emplace_back(std::make_unique<B>());

  for (auto &x : xs) {
    x->print("hi");
  }

  return 0;
}
