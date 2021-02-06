#include <iostream>

std::string &f() {
  std::string s = "Example";
  return s;
}

int main(void) {

  std::string &r = f();        // dangling reference.
  std::cout << r << std::endl; // ub, read from dangling ref
  std::string s = f();         // ub, copy initialize from dangling ref.

  return 0;
}
