#include <functional>
#include <iostream>
#include <type_traits>

using namespace std;

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

struct S {
  int i;
  S(int i) : i(i) {}
};

int main(void) {
  auto val = foo([](int a, int b) -> double { return a + b; })(1, 2);
  auto fn = [](int a) -> S { return S{1}; };
  lambda_traits<decltype(fn)>::return_type v{1};

  std::cout << v.i << std::endl;

  return 0;
}
