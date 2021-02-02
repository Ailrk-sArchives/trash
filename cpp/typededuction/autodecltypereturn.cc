#include <deque>
#include <iostream>

template <typename Container, typename I>
auto auth_and_access1(Container &c, I i) {
  return c[i];
}

template <typename Container, typename I>
auto auth_and_access2(Container &c, I i) -> decltype(c[i]) {
  return c[i];
}

template <typename Container, typename I>
decltype(auto) auth_and_access3(Container &c, I i) {
  return c[i];
}

int main(void) {
  std::deque<int> d{1, 2, 3};

  // here type of c[i] is deduced by template type deduction.
  // and the referencenss is dropped.
  int a = auth_and_access1(d, 2);

  decltype(auto) a1 = std::move(a);
  auto a2 = std::move(a);

  // by using a decltype you specify the return type to be
  // exactly the same a the expreesion you passed in.
  auth_and_access2(d, 2) = 10;

  return 0;
}
