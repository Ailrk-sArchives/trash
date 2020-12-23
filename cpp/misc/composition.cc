#include <functional>
#include <iostream>
#include <vector>

template <typename... P> struct parameter_pack {
  template <template <typename...> typename T> using apply = T<P...>;
};

template <typename T> struct memfun_type { using type = void; };

template <typename Ret, typename Class, typename... Args>
struct memfun_type<Ret (Class::*)(Args...) const> {
  using type = std::function<Ret(Args...)>;
  using return_type = Ret;

  template <template <typename...> typename F>
  using args_pack = typename parameter_pack<Args...>::template apply<F>;
};

template <typename F> struct lambda_traits {
  using type = typename memfun_type<decltype(&F::operator())>::type;
  using return_type =
      typename memfun_type<decltype(&F::operator())>::return_type;

  // F2 is a hkt that takes ...Args as parameter
  template <template <typename...> typename F2>
  using args_pack =
      typename memfun_type<decltype(&F::operator())>::template args_pack<F2>;
};

template <typename Fn, typename... Ts>
typename lambda_traits<Fn>::type foo(Fn fn) {
  return fn;
}

template <typename Fn1, typename Fn2> struct composition {
  template <typename... Args2>
  using p_ = std::function<typename lambda_traits<Fn1>::return_type(Args2...)>;
  using type = typename lambda_traits<Fn2>::template args_pack<p_>;
};

std::function<char(int)> f1{[](int a) { return 'a'; }};
std::function<int(double)> f2{[](double b) { return b + 100 % 10; }};
std::function<std::vector<char>(char)> f3{[](char a) {
  return std::vector<char>{a, a, a};
}};

template <typename Fn1, typename Fn2>
typename composition<Fn1, Fn2>::type operator<(Fn1 f, Fn2 g) {
  return [=](auto a) { return f(g(a)); };
}

composition<decltype(f1), decltype(f2)>::type f1f2(f1 < f2);
composition<decltype(f3), decltype(f1f2)>::type f3f1f2(f3 < f1f2);
auto f3f1f2_ = f3 < (f1 < f2);

int main(void) {
  auto a = f1f2(1.1);

  // this should type checks.
  typename lambda_traits<
      composition<decltype(f3), decltype(f2)>::type>::return_type b =
      std::vector<char>{'a', 'a'};

  std::cout << f3f1f2(1.1).size() << std::endl;
  std::cout << f3f1f2_(1.1).size() << std::endl;

  std::cout << a << std::endl;

  return 0;
}
