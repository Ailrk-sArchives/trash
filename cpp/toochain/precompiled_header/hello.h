#pragma once
#include <iostream>

template <size_t N> struct fib {
  const static size_t value = fib<N - 1>::value + fib<N - 2>::value;
};
template <> struct fib<1> { const static size_t value = 1; };
template <> struct fib<0> { const static size_t value = 1; };

template <typename T> void say(T t) {
  auto value =
      fib<900>::value + fib<900>::value + fib<900>::value + fib<900>::value;
  std::cout << value << std::endl;
  std::cout << "  " << t << std::endl;
}

