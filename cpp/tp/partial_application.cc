#include <iostream>
#include <string>

// varadic template and fold expression
//
// arguments of varadic tempalte is called parameter pack.
// it means to accepts zero or more tempalte arguments.
// Template arguments can be either types or values

// A function parameter pack is a function parameter that takes
// zero or more function arguments.

template <typename... Ts> struct Tuple {};
template <typename T1, typename T2> struct Pair {};

Tuple<> t0;
Tuple<int, int> t1;
Tuple<double, float> t2;

// - implement zip with parameter pack and pack extension
template <typename... Ts> struct zip {
  template <typename... Us> struct with {
    using type = Tuple<Pair<Ts, Us>...>;
  };
};

zip<int, int, int>::with<double, double, char>::type x;
zip<std::uint32_t, uint32_t>::with<std::int16_t, std::int16_t>::type y;

template <typename... Args> void f(Args... args) {}
template <typename... Args> void h(Args... args) {}

// - nested pack extension
template <typename... Args> void g(Args... args) {
  int n = 0;
  f(const_cast<const Args *>(args)...);

  // this results to f(h(T1, T2, T3...) + T1, h(T1, T2, T3...) + T2, ...);
  // the inner pattern is expanded first, then it is combined with the
  // outer pattern for the next expansion.
  f(h(args...) + args...);

  f(&args...);
  f(n, ++args...);
  f(++args..., n);
  f(std::forward(args)...);
  f(std::move(n), std::move(args)...);
}

template <typename T, typename... Ts>
auto mk_namedtuple(std::string name, Ts... args) {
  std::cout << name << " " << T() << std::endl;
  return Tuple<Ts...>{};
}

// note sizeof... is an operator only for parameter pack.
// here the semantic is how many parameters are there.
template <typename... Ts> auto partial_add3(Ts... xs) {
  static_assert(sizeof...(xs) <= 3);
  if constexpr (sizeof...(xs) == 3) {
    return (0 + ... + xs);
  } else {
    return [xs...](auto... ys) { return partial_add3(xs..., ys...); };
  }
}

auto a1 = partial_add3(1, 3, 4);
auto a2 = partial_add3(1, 3)(4);
