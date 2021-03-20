#include <iostream>

// use case of reinterpret case to check endianess.
bool is_little_endian() {
  // big endianess:    0000 0000 0000 0001
  // little endianess: 0000 0001 0000 0000
  std::uint16_t x = 0x0001;
  auto p = reinterpret_cast<std::uint8_t *>(&x);

  // if it's little endianess, *p will be the first 8 0 bits.
  return *p != 0;
}

int main() {

  int a = 1000;

  std::cout << "static cast 1000 to: doube, value is:" << static_cast<double>(a)
            << std::endl;

  // in memory it's still the representation of 1000, but the memory is treated
  // as double now.
  // you can't directly reinterpret cast int to double, but you can cast an
  // int pointer to double pointer then dereference it.
  std::cout << "reinterpret cast 1000 to: doube, value is:"
            << *reinterpret_cast<double *>(&a) << std::endl;
}
