#include <cassert>
#include <iostream>

// we write a generic function to
// swap two bytes.
template <typename T> T byte_swap(T value) {
  unsigned char *bytes = reinterpret_cast<unsigned char *>(&value);
  for (size_t i = 0; i < sizeof(T); i += 2) {
    unsigned char v = std::move(bytes[i]);
    bytes[i] = std::move(bytes[i + 1]);
    bytes[i + 1] = std::move(v);
  }
}

// the problem is we don't want these function perform on
// all types. For instance, it doesn't make sense to
// swap bytes of a double, or a char.
// Specially char, you only have 1 bytes length, and swap
// will swap the content of the char with a value in some
// random memory location.

// but you can specialize implementation for certain types.
template <> double byte_swap(double value) {
  assert(false && "Illegal to swap double");
  return value;
}

template <> char byte_swap(char value) {
  assert(false && "Illegal to swap char");
  return value;
}
