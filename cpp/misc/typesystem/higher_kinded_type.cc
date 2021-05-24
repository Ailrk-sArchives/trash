#include <concepts>

template <template <typename> typename Con, typename Arg>
using apply = Con<Arg>;

template <typename> struct A {};
template <typename> struct B {};

int main(void) {
  using x = apply<A, int>;
  using y = apply<B, double>;

  static_assert(std::same_as<x, A<int>>);
  static_assert(std::same_as<y, B<double>>);
  return 0;
}
