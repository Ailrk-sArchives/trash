#include <functional>
#include <iostream>
#include <vector>

template <typename... P> struct parameter_pack {
  template <template <typename...> typename T> using apply = T<P...>;
};

template <typename T> struct lambda_traits_impl { using type = void; };

template <typename Ret, typename Class, typename... Args>
struct lambda_traits_impl<Ret (Class::*)(Args...) const> {
  using type = std::function<Ret(Args...)>;
  using return_type = Ret;

  template <template <typename...> typename F> // all arguments
  using args_pack = typename parameter_pack<Args...>::template apply<F>;

  template <template <typename...> typename F> // arguments and return type.
  using all_pack = typename parameter_pack<Args..., Ret>::template apply<F>;
};

template <typename F> struct lambda_traits {
  using type = typename lambda_traits_impl<decltype(&F::operator())>::type;
  using return_type =
      typename lambda_traits_impl<decltype(&F::operator())>::return_type;

  // F2 is a hkt that takes ...Args as parameter
  template <template <typename...> typename F2>
  using args_pack = typename lambda_traits_impl<decltype(
      &F::operator())>::template args_pack<F2>;

  template <template <typename...> typename Fn>
  using all_pack = typename lambda_traits_impl<decltype(
      &F::operator())>::template all_pack<Fn>;
};

template <typename Fn1, typename Fn2> struct composition {
  template <typename... Args2>
  using p_ = std::function<typename lambda_traits<Fn1>::return_type(Args2...)>;
  using type = typename lambda_traits<Fn2>::template args_pack<p_>;
};

// template <typename Fn1, typename Fn2> decltype(auto) operator<(Fn1 f, Fn2 g)
// {
//   return [=](auto &&... a) { return f(g(a...)); };
// }

template <typename Fn1, typename Fn2> auto operator<(Fn1 f, Fn2 g) {
  return [=](auto &&...a) { return f(g(a...)); };
}

int main(void) {
  auto f = [](int a) { return a + 1; } <
           [](std::vector<int> v) { return v.size(); };
  std::cout << f(std::vector<int>{1, 2}) << std::endl;

  auto g = [](int a) { return a + 1.1; };
  auto h = [](std::vector<int> x, double y) { return x.size() + y; };

  auto gh = g < h;

  auto x = gh(std::vector<int>{1, 2}, 2);
  std::cout << x << std::endl;

  return 0;
}
