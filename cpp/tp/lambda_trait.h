#include <functional>
#include <iostream>
#include <type_traits>

using namespace std;

template <size_t n, typename...> struct elt_ { using at = void; };
template <typename T, typename... Ts> struct elt_<0, T, Ts...> {
  using at = T;
};
template <size_t n, typename T, typename... Ts> struct elt_<n, T, Ts...> {
  using at = typename elt_<n - 1, Ts...>::at;
};

template <typename... Ts> struct elt {
  template <size_t n> using at = typename elt_<n, Ts...>::at;
};

template <typename T> struct memfun_type { using type = void; };

template <typename Ret, typename Class, typename... Args>
struct memfun_type<Ret (Class::*)(Args...) const> {
  using type = std::function<Ret(Args...)>;
  using return_type = Ret;

  template <size_t n> using parameter = typename elt<Args...>::template at<n>;
};

template <typename F> struct lambda_traits {
  using type = typename memfun_type<decltype(&F::operator())>::type;
  using return_type =
      typename memfun_type<decltype(&F::operator())>::return_type;

  template <size_t n>
  using parameter =
      typename memfun_type<decltype(&F::operator())>::template parameter<n>;
};

auto foo = [](int, double, char) -> int { return 1; };

#define TEST
#ifdef TEST
static_assert(std::is_same_v<lambda_traits<decltype(foo)>::return_type, int>);
static_assert(std::is_same_v<lambda_traits<decltype(foo)>::parameter<0>, int>);
static_assert(
    std::is_same_v<lambda_traits<decltype(foo)>::parameter<1>, double>);
static_assert(std::is_same_v<lambda_traits<decltype(foo)>::parameter<2>, char>);
static_assert(
    !std::is_same_v<lambda_traits<decltype(foo)>::parameter<2>, double>);
#endif
