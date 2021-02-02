#include <iostream>
#include <memory>
#include <vector>

struct Parameters {
  int r1;
  int r2;
  std::unique_ptr<std::vector<int>> vs;
};

int foo(Parameters params) { return params.vs->size(); }

int main(void) {

  auto vs = std::make_unique<std::vector<int>>(std::vector<int>{1, 2, 3});
  auto res = foo({.r1 = 10, .r2 = 20, .vs = std::move(vs)});
  std::cout << res << std::endl;

  return 0;
}
