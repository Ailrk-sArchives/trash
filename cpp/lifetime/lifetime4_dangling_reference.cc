#include <functional>
#include <iostream>
#include <string_view>

// return a dangling reference.
const int &get_data_dangling() {
  const int i = 5;
  return i;
}

// return a reference wrapper.
std::reference_wrapper<const int> get_data_ref_wrapper() {
  const int i = 5;
  return std::ref(i);
}

// "hello" is in the static section.
// static duration.
const char *get_string_literal() { return "hello"; }

// same as above
std::string_view get_string_view1() { return "Hello World"; }

// now the pointer point to s with automatic duration.
// this one is ub.
std::string_view get_string_view2() {
  std::string s = "Hello World";
  return s;
}

// local array initialized by a global data.
// this is UB but no warning from the compiler.
std::string_view get_string_local() {
  const char s[] = "hello world";
  return s;
}

int main(void) {
  // how this work? what is a reference wrapper?
  std::cout << get_string_local() << std::endl;
  return 1;
}
