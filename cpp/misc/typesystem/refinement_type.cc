

template <auto x> auto f() requires(x >= 42) {}

int main(void) {
  f<42>();
  // f<10>();
  return 0;
}
