#include <iostream>

namespace tpgcd {
/* gcd :: Int -> Int -> Int
 * gcd a 0 = a
 * gcd a b = gcd b $ a % b
 */
template <int a, int b> struct gcd {
  static constexpr int value = gcd<b, a % b>::value;
};
template <int a> struct gcd<a, 0> { static constexpr int value = a; };

void run() { std::cout << "GCD: " << gcd<25, 50>::value << std::endl; }
} // namespace tpgcd

namespace tpfib {
/* fib :: Int -> Int
 * fib 0 = 1
 * fib 1 = 1
 * fib i = (fib $ i - 1) $ (fib $ i - 2)
 */
template <int i> struct fib {
  static constexpr int value = fib<i - 1>::value + fib<i - 2>::value;
};
template <> struct fib<1> { static constexpr int value = 1; };
template <> struct fib<0> { static constexpr int value = 1; };

void run() { std::cout << "FIB: " << fib<5>::value << std::endl; }
} // namespace tpfib

namespace tpif {
/* data Union a b = a | b
 * if :: Bool -> a -> b -> Union a b
 * if false a b = b
 * if true a b = a
 */
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
  static inline void exec() { std::cout << "yikes" << std::endl; }
};

struct D {
  static inline void exec() { std::cout << "yummies" << std::endl; }
};

auto run() -> void {
  std::cout << If<true, A, B>::test::value << std::endl;
  std::cout << If<false, A, B>::test::value << std::endl;
  If<false, C, D>::test::exec();
}
} // namespace tpif

namespace tplist {
/* data List = Nil | Cons a (List a)
 */
struct Nil {};
template <int a, typename L> struct List {
  static constexpr int head = a;
  using tail = L;
};

using mylist = List<10, List<100, List<2, Nil>>>;

/* sum :: (Semiring a) => List a -> a
 * sum [] = 0
 * sum (x:xs) = x + (sum xs)
 */
template <typename...> struct sum { static constexpr int value = 0; };
template <int a, typename Tail> struct sum<List<a, Tail>> {
  static constexpr int value = a + sum<Tail>::value;
};

/* foldr
 */
template <typename L, template <int> typename F, template <int, int> typename R,
          int Base>
struct foldr {
  static constexpr int value = Base;
};
template <int a, typename Tail, template <int> typename F,
          template <int, int> typename R, int Base>
struct foldr<List<a, Tail>, F, R, Base> {
  static constexpr int value =
      R<F<a>::value, foldr<Tail, F, R, Base>::value>::value;
};

} // namespace tplist

int main(void) {
  std::cout << tpgcd::gcd<28, 100>::value << std::endl;
  std::cout << tpfib::fib<10>::value << std::endl;
  tpif::run();
  std::cout << "sum: " << tplist::sum<tplist::mylist>::value << std::endl;

  return 0;
}
