#include <iostream>

// The most basic type trait just turn on for some
// types and turn off for the others.

// we want byte_swap only be avaible on certain types
template <typename T> struct is_swappable {
  static const bool value = false; // default false
};

template <> struct is_swappable<unsigned short> {
  static const bool value = true;
};

template <> struct is_swappable<short> { static const bool value = true; };

template <> struct is_swappable<unsigned long> {
  static const bool value = true;
};

template <> struct is_swappable<long> { static const bool value = true; };

template <> struct is_swappable<unsigned long long> {
  static const bool value = true;
};

template <> struct is_swappable<long long> { static const bool value = true; };

// we write a generic function to
// swap two bytes.
template <typename T, typename = std::enable_if_t<is_swappable<T>::value>>
T byte_swap(T value) {
  unsigned char *bytes = reinterpret_cast<unsigned char *>(&value);
  for (size_t i = 0; i < sizeof(T); i += 2) {
    unsigned char v = std::move(bytes[i]);
    bytes[i] = std::move(bytes[i + 1]);
    bytes[i + 1] = std::move(v);
  }
  return value;
}

int main(void) {
  short s = 12;
  double d = 1.2;

  std::cout << byte_swap(s) << std::endl;
  // std::cout << byte_swap(d) << std::endl;

  return 0;
}
