// continuation with c++
// some experiments.

#include <functional>
#include <iostream>
#include <type_traits>

template <typename T> struct memfun_type { using type = void; };

template <typename Ret, typename Class, typename... Args>
struct memfun_type<Ret (Class::*)(Args...) const> {
  using type = std::function<Ret(Args...)>;
  using return_type = Ret;
};

template <typename F>
typename memfun_type<decltype(&F::operator())>::type
to_function(F const &func) { // Function from lambda !
  return func;
}

template <typename F> struct lambda_traits {
  using type = typename memfun_type<decltype(&F::operator())>::type;
  using return_type =
      typename memfun_type<decltype(&F::operator())>::return_type;
};

template <typename Fn, typename... Ts>
typename lambda_traits<Fn>::type foo(Fn fn) {
  return fn;
}

// find a way to simplify this.
template <typename T, typename Fn>
typename lambda_traits<Fn>::return_type tripleCPS(T a, Fn k) {
  return k(a * a * a);
}

template <typename T, typename Fn,
          typename R = typename lambda_traits<Fn>::return_type>
R factCPS(T a, Fn k) {
  if (a <= 0) {
    return k(1);
  } else {
    return factCPS(a - 1, [&](T b) { return k(a * b); });
  }
};

template <typename T, typename Fn,
          typename R = typename lambda_traits<Fn>::return_type>
R stringCPS(T a, Fn k) {
  return k("string");
}

template <typename T, typename Fn,
          typename R = typename lambda_traits<Fn>::return_type>
R doubleCPS(T a, Fn k) {
  static_assert(std::is_convertible_v<Fn, std::function<R(T)>>);
  return k(2 * a);
}

template <typename T, typename Fn,
          typename R = typename lambda_traits<Fn>::return_type>
R absCPS(T a, Fn k) {
  static_assert(std::is_convertible_v<Fn, std::function<R(T)>>);
  return k(-a);
}

template <typename T, typename Fn,
          typename R = typename lambda_traits<Fn>::return_type>
R lengthCPS(T a, Fn k) {
  static_assert(std::is_convertible_v<Fn, std::function<R(int)>>);
  return k(a.size());
}

template <typename T, typename Fn,
          typename R = typename lambda_traits<Fn>::return_type>
std::function<R(T)> makeCPS(Fn g) {
  return [](auto x, auto g) { return g(f(x)); };
}

auto id = [](auto a) { return a; };

int main(void) {
  // because in C++ there is no global inference, R cannot be inferred as it's
  // not a concrete type.

  auto a = doubleCPS(10, [](int a) {
    return doubleCPS(a, [](int b) {
      return stringCPS(b, [](std::string c) {
        return lengthCPS(c, [](int sz) { return sz; });
      });
    });
  });

  std::cout << a << std::endl;
  return 0;
}
