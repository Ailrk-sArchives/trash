#include <iostream>
#include <memory>

// get a rference of the pointer
template <typename T> void side_effect_with_ref(T &ptr) { (*ptr)++; }

// pass the pointer by value directly.
template <typename T> void side_effect_with_ptr(T *ptr) { (*ptr)++; }

int main(void) {
  auto ptr = std::make_unique<int>(10);
  std::cout << "first: " << *ptr << std::endl;
  side_effect_with_ref(ptr);
  side_effect_with_ptr(ptr.get());

  std::cout << "second: " << *ptr << std::endl;

  return 0;
}
