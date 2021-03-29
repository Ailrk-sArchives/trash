#include "lambda_trait.h"
#include <iostream>
#include <type_traits>

template <typename T> struct F {
  T value;
  using value_type = T;

  template <typename Fn, typename U = typename lambda_traits<Fn>::return_type>
  F<U> map(Fn fn);
};

template <typename T> template <typename Fn, typename U> F<U> F<T>::map(Fn fn) {
  U u = fn(value);
  return F<U>{u};
}

int foo() { return 1; }

static_assert(std::is_same_v<std::invoke_result_t<decltype(foo)>, int>);

int main(void) {
  F<int> f{1};
  F<double> f1 = f.map([](int a) { return 1.1; });
  return 0;
}
