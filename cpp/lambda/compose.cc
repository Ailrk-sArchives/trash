
#include <vector>
#include <iostream>

auto compose(auto f, auto g) {
  return [=](auto &&... x) { return f(g(x...)); };
};



double foo(int a, char b) {
  return a + static_cast<int>(b);
}


int bar(std::vector<int> v) {
  return v.size();
}


int main(void)
{

  auto f = compose(foo, bar);
  return 0;
}
