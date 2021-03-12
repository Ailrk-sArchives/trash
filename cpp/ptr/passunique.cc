#include <iostream>
#include <memory>

// get a rference of the pointer
template <typename T> void side_effect_with_ref(T &ptr) { (*ptr)++; }

template <typename T> void side_effect_with_copy(T ptr) { (*ptr)++; }

template <typename T> void side_effect_with_move(T &&ptr) { (*ptr)++; }
// pass the pointer by value directly.
template <typename T> void side_effect_with_ptr(T *ptr) { (*ptr)++; }

template <typename T> void side_effect_with_ref_to_val(T &val) { val++; }

int main(void) {
  auto ptr = std::make_unique<int>(10);
  std::cout << "first: " << *ptr << std::endl;
  side_effect_with_ref(ptr);

  // can not do this.
  // side_effect_with_copy(ptr);

  side_effect_with_ptr(ptr.get());

  side_effect_with_ref_to_val(*ptr);

  side_effect_with_move(std::move(ptr));

  std::cout << "second: " << *ptr << std::endl;

  std::cout << "ptr after moved: " << *ptr << std::endl;

  return 0;
}
