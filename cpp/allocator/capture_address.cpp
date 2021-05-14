#include <cstdio>
#include <iostream>
#include <string>

struct Vector3 {
  int x;
  int y;
  int z;

  std::string to_string() const {
    char buf[124];
    sprintf(buf, "<Vector3 %p>", this);
    return std::string(buf);
  }
};

int main(void) {
  Vector3 v{1, 2, 3};

  std::cout << v.to_string() << std::endl;

  // by reference get the same address.
  auto foo = [&]() { std::cout << v.to_string() << std::endl; };

  // by value the addres + f for some reason.
  auto bar = [=]() { std::cout << v.to_string() << std::endl; };

  foo();
  bar();
  return 0;
}
