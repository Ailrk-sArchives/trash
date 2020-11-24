#include <functional>
#include <iostream>
#include <string>

int main(void) {
  auto f = [](int n) -> std::function<int(std::string)> {
    return [=](std::string s) { return s.size() + n; };
  };

  auto x = f(1)(std::string("asd"));
  std::cout << x << std::endl;


  return 0;
}
