#include <iostream>

// how the fuck this works so well.

auto doubleCPS = [](double a, auto k) -> decltype(auto) { return k(a * 2); };

auto absCPS = [](double a, auto k) -> decltype(auto) { return k(std::abs(a)); };

auto id = [](auto a) { return a; };

auto stringCPS = [](auto a, auto k) -> decltype(auto) {
  return k(std::string("string"));
};

auto sizeCPS = [](auto a, auto k) -> decltype(auto) { return k(a.size()); };

auto makeCPS = [](auto f) { return [=](auto x, auto g) { return g(f(x)); }; };

auto cps1 = makeCPS([](auto a) { return a + 99; });

int main(void) {

  auto a = doubleCPS(10, [](int a) {
    return stringCPS(a, [](auto b) {
      return sizeCPS(b, [](int sz) {
        return doubleCPS(
            sz, [](double v) { return cps1(v, [](double x) { return x; }); });
      });
    });
  });
  std::cout << a << std::endl;
  return 0;
}
