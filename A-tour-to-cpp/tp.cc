#include <iostream>

namespace tpgcd {
template <int a, int b> struct gcd {
  static constexpr int value = gcd<b, a % b>::value;
};
template <int a> struct gcd<a, 0> { static constexpr int value = a; };

void run() { std::cout << "GCD: " << gcd<25, 50>::value << std::endl; }
} // namespace tpgcd

namespace tpfib {
template <int i> struct fib {
  static constexpr int value = fib<i - 1>::value + fib<i - 2>::value;
};
template <> struct fib<1> { static constexpr int value = 1; };
template <> struct fib<0> { static constexpr int value = 1; };

void run() { std::cout << "FIB: " << fib<5>::value << std::endl; }
} // namespace tpfib

namespace tpif {
template <bool C, typename Then, typename Else> struct If {};
template <typename Then, typename Else> struct If<false, Then, Else> {
  using test = Else;
};
template <typename Then, typename Else> struct If<true, Then, Else> {
  using test = Then;
};

struct A {
  static constexpr int value = 1;
};

struct B {
  static constexpr int value = 0;
};

struct C {
  static inline void exec() { std::cout << "yikes"; }
};

struct D {
  static inline void exec() { std::cout << "yummies"; }
};

auto run() -> void {
  std::cout << If<true, A, B>::test::value << std::endl;
  std::cout << If<false, A, B>::test::value << std::endl;
  If<false, C, D>::test::exec();
}
} // namespace tpif

namespace tplist {
struct Nil {};

template <int a, typename L> struct List {
  static constexpr int head = a;
  using tail = L;
};

using mylist = List<'a', List<'b', List<'c', Nil>>>;

template <typename L> struct sum {
  static constexpr int value = 0;
};
template <int a, typename Tail> struct sum<List<a, Tail>> {
  static constexpr int value = a + sum<Tail>::value;
};

}

int main(void) {
  std::cout << tpgcd::gcd<28, 100>::value << std::endl;
  std::cout << tpfib::fib<10>::value << std::endl;
  tpif::run();

  return 0;
}
