#include <iostream>

#define TESTING
// rule of thumb:
// meta functions should take thunks as argument whenever possible.

template <typename T> struct identity { using type = T; };

template <int N> struct int_ : identity<int_<N>> {
  static const int value = N;
};

template <char N> struct char_ : identity<char_<N>> {
  static const int value = N;
};

template <bool N> struct bool_ : identity<bool_<N>> {
  static const int value = N;
};

struct make_const : identity<make_const> { // higher order function
  template <typename T> using apply = const T;
};

template <typename A, typename B>
struct add : int_<A::type::value + B::type::value> {};

template <typename A, typename B>
struct minus : int_<A::type::value - B::type::value> {};

template <typename A, typename B>
struct times : int_<A::type::value * B::type::value> {};

template <typename C, typename A, typename B> struct if_;
template <bool C, typename A, typename B> struct if_impl_;
template <typename T, typename F> struct if_impl_<true, T, F> : T {};
template <typename T, typename F> struct if_impl_<false, T, F> : F {};
template <typename C, typename T, typename F>
struct if_ : if_impl_<C::type::value, T, F> {};

template <typename A, typename B>
struct less : bool_<(A::type::value < B::type::value)> {};
template <typename A, typename B>
struct less_equal : bool_<(A::type::value <= B::type::value)> {};
template <typename A, typename B>
struct greater : bool_<(A::type::value > B::type::value)> {};
template <typename A, typename B>
struct greater_equal : bool_<(A::type::value >= B::type::value)> {};

template <typename T> struct alreay_lazy;
template <typename Exp> struct lazy : Exp {};
template <typename T> struct lazy<alreay_lazy<T>> { using type = T; };
template <template <typename...> typename F, typename... Ts>
struct lazy<F<Ts...>> : F<typename lazy<Ts>::type...> {};

// curry<times>::apply<int_<1>>::apply<int_<2>>::type

#ifdef TESTING
namespace app {
template <typename N>
struct fact : lazy<typename

                   if_<

                       typename less<N, int_<1>>::type,

                       int_<1>,

                       times<N, fact<minus<N, int_<1>>>>

                       >::type> {};

constexpr int fact10 = fact<int_<10>>::value;

} // namespace app
#endif

int main(void) {

#ifdef TESTING
  std::cout << app::fact10 << std::endl;
#endif
  return 0;
}
