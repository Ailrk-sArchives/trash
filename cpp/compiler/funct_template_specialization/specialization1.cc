#include <atomic>
#include <cassert>
#include <iostream>

template <typename T> T byte_swap(T value) {
  unsigned char *bytes = reinterpret_cast<unsigned char *>(&value);
  for (size_t i = 0; i < sizeof(T); i += 2) {
    unsigned char v = std::move(bytes[i]);
    bytes[i] = std::move(bytes[i + 1]);
    bytes[i + 1] = std::move(v);
  }
}

double byte_swap(double value) {
  assert(false && "Illegal to swap double");
  return value;
}

char byte_swap(char value) {
  assert(false && "Illegal to swap char");
  std::cout << "good" << std::endl;
  return value;
}

template <> char byte_swap(char value) {
  std::atomic<int> counter;
  assert(false && "Illegal to swap char");
  for (int i = 0; i < 10; ++i) {
    counter++;
  }
  return value + counter;
}

int main(void) {
  char c = 'a';
  char c1 = byte_swap(c);
  char c2 = byte_swap<>(c);
  return 0;
}
