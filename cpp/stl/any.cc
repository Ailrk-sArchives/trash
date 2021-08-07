#include <any>
#include <iostream>

struct Good {};
struct Bad {};
struct Ugly {};

// any is a dynamic value.
int main(void) {
  // good cast
  std::any a = 1;
  std::cout << a.type().name() << ": " << std::any_cast<int>(a) << std::endl;
  a = 3.14;
  std::cout << a.type().name() << ": " << std::any_cast<double>(a) << std::endl;
  a = true;
  std::cout << a.type().name() << ": " << std::any_cast<bool>(a) << std::endl;

  try {
    a = 1;
    std::cout << std::any_cast<float>(a) << std::endl;
  } catch (const std::bad_any_cast &e) {
    std::cout << e.what() << std::endl;
  }

  a = 1;
  if (a.has_value()) {
    std::cout << a.type().name() << std::endl;
  }

  a.reset();
  if (!a.has_value()) {
    std::cout << "novalue" << std::endl;
  }

  a = Good();
  std::cout << a.type().name() << std::endl;
  a = Bad();
  std::cout << a.type().name() << std::endl;
  a = Ugly();
  std::cout << a.type().name() << std::endl;

  a = 1;
  int *i = std::any_cast<int>(&a);
  std::cout << *i << std::endl;
  return 0;
}
