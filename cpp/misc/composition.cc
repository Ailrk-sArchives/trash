#include <functional>
#include <iostream>
#include <vector>
using std::function;
using std::vector;

template <typename... P> struct parameter_pack {
  template <template <typename...> typename T> using apply = T<P...>;
};

template <typename T> struct memfun_type { using type = void; };

template <typename Ret, typename Class, typename... Args>
struct memfun_type<Ret (Class::*)(Args...) const> {
  using type = std::function<Ret(Args...)>;
  using return_type = Ret;

  template <template <typename...> typename F> // all arguments
  using args_pack = typename parameter_pack<Args...>::template apply<F>;

  template <template <typename...> typename F> // arguments and return type.
  using all_pack = typename parameter_pack<Args..., Ret>::template apply<F>;
};

template <typename F> struct lambda_traits {
  using type = typename memfun_type<decltype(&F::operator())>::type;
  using return_type =
      typename memfun_type<decltype(&F::operator())>::return_type;

  // F2 is a hkt that takes ...Args as parameter
  template <template <typename...> typename F2>
  using args_pack =
      typename memfun_type<decltype(&F::operator())>::template args_pack<F2>;

  template <template <typename...> typename Fn>
  using all_pack =
      typename memfun_type<decltype(&F::operator())>::template all_pack<Fn>;
};

template <typename Fn, typename... Ts>
typename lambda_traits<Fn>::type foo(Fn fn) {
  return fn;
}

// composition
template <typename Fn1, typename Fn2> struct composition {
  template <typename... Args2>
  using p_ = std::function<typename lambda_traits<Fn1>::return_type(Args2...)>;
  using type = typename lambda_traits<Fn2>::template args_pack<p_>;
};

template <typename Fn1, typename Fn2>
typename composition<Fn1, Fn2>::type operator<(Fn1 f, Fn2 g) {
  return [=](auto &&... a) { return f(g(a...)); };
}


template <typename Fn1, typename Fn2> auto operator>(Fn1 f1, Fn2 f2) {
  return [=](auto ...a) { return f1(f2(a...)); };
}
// currying
template <typename...> struct currying_impl { using type = void; };
template <typename Arg, typename R> struct currying_impl<Arg, R> {
  using type = std::function<R(Arg)>;
};

template <typename Arg, typename... Args> struct currying_impl<Arg, Args...> {
  using type = std::function<typename currying_impl<Args...>::type(Arg)>;
};

template <typename Fn> struct curring {
  // funciotn<r(a, b, c...)> -> template <typename a, b, c, r>
  using type =
      typename lambda_traits<Fn>::template all_pack<currying_impl>::type;
};

template <typename Fn> using curring_t = typename curring<Fn>::type;

curring_t<std::function<int(double, char, vector<int>)>> f;
auto a = f(1.1);
auto b = a('1');
auto x = b({1, 2, 3});

// test for composition
function<char(int)> f1{[](int a) { return 'a'; }};
function<int(double)> f2{[](double b) { return b + 100 % 10; }};
function<vector<char>(char)> f3{[](char a) { return vector<char>{a, a, a}; }};
function<int(int, double)> f5{[](int b, double c) { return b + 100 % 10 + c; }};

composition<decltype(f1), decltype(f2)>::type f1f2(f1 < f2);
composition<decltype(f3), decltype(f1f2)>::type f3f1f2(f3 < f1f2);

auto f3f1f2_ = f3 < (f1 < f2);
auto f3f1f2__ = f3 > (f1 > f2);


auto c = [](auto x) { return f1(f2(x)); };

int main(void) {
  auto a = f1f2(1.1);

  // this should type checks.
  typename lambda_traits<
      composition<decltype(f3), decltype(f2)>::type>::return_type b =
      std::vector<char>{'a', 'a'};
  auto fx = [](int a) { return a + 1.1; } < [](char c) { return (int)c; };
  auto fx_r = fx('a');

  auto x1 = c(1.1);

  std::cout << f3f1f2(1.1).size() << std::endl;
  std::cout << f3f1f2_(1.1).size() << std::endl;
  std::cout << f3f1f2__(1.1).size() << std::endl;

  std::cout << a << std::endl;

  return 0;
}
