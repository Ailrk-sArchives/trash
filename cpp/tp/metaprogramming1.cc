#include <type_traits>

/* This version shows all bad practices... */

// template meta function.
// template meta function should always take a type as paramter.

template <typename T> struct make_const_ { using type = const T; };
make_const_<int>::type a = 1;

// identity
template <typename T> struct identity { using type = T; };

// Box value into type so it works with our convention.
// use curisou recurring pattern to add ::type refer to itself.
template <int N> struct int_ : identity<int_<N>> {
  static const int value = N;
};
template <bool N> struct bool_ : std::integral_constant<bool, N> {};
template <char N> struct char_ : std::integral_constant<char, N> {};

template <typename A, typename B> struct times {
  using type = int_<A::type::value * B::type::value>;
};
template <typename A, typename B> struct add {
  using type = int_<A::type::value + B::type::value>;
};
template <typename A, typename B> struct minus {
  using type = int_<A::type::value - B::type::value>;
};

constexpr bool H_less_then_(int a, int b) { return a < b; }
constexpr bool H_greater_then_(int a, int b) { return a > b; }

template <typename A, typename B> struct less {
  using type = int_<H_less_then_(A::type::value, B::type::value)>;
};

template <typename A, typename B> struct greater {
  using type = int_ < A::value<H_greater_then_(A::type::value, B::type::value)>;
};

template <typename A, typename B> struct equal {
  using type =
      int_ < A::value<A::type::value == B::type::value ? A::type::value
                                                       : B::type::value>;
};

constexpr int c = times<int_<1>, int_<2>>::type::value;
static_assert(c == 2);

// just inherits. The technique is called meta function forwarding.
template <typename A> struct double_n : times<A, int_<2>> {};

constexpr int c1 = double_n<times<int_<1>, int_<2>>::type>::type::value;
static_assert(c1 == 4);

////////////////////////////////////////////////////////////////////////////////
// higher order meta function
// meta functions take type as argument, but meta functions themselves are
// templates, not type. to work around we box them in another type.

struct make_const_volatile { // a template meta function class.
  using type = make_const_volatile;
  template <typename T> struct apply { using type = const volatile T; };
};

struct make_const {
  using type = make_const;
  template <typename T> struct apply { using type = const T; };
};

// taking higher order function
template <typename F> struct transform_int_type : F::template apply<int> {};

using cv_int = transform_int_type<make_const_volatile>::type;
using c_int = transform_int_type<make_const>::type;
cv_int c2 = 1;

////////////////////////////////////////////////////////////////////////////////
// use lazyness to implement conditional

template <typename C, typename T, typename F> struct if_;

// use lazyness for conditiona
template <bool C, typename T, typename F> struct if_impl_ { using type = T; };
template <typename T, typename F> struct if_impl_<true, T, F> {
  using type = T;
};
template <typename T, typename F> struct if_impl_<false, T, F> {
  using type = F;
};

// we pass T and F without evaluate them.
template <typename C, typename T, typename F>
struct if_ : if_impl_<C::value, T, F> {};

constexpr int c3 = times<int_<2>, if_<

                                      bool_<true>,

                                      int_<2>, int_<3>>::type>::type::value;
static_assert(c3 == 4);

//  control lazyness and strictness.
namespace nono {
template <typename N>
struct fact_nono :

    if_<typename less<N, int_<1>>::type,

        int_<1>,

        typename times<

            // this doesnt' work!!!
            // the value will returned by if, which is unevalutated.
            typename fact_nono<typename minus<N, int_<1>>::type>::type,

            N>::type

        > {};

// constexpr int c5 = fact_nono<int_<0>>::type::value;
} // namespace nono

// box and unbox
// the point is even if it's a value, it still has a ::type member to be
// accessed.
template <typename T> struct box { using type = box; };
template <typename T> struct unbox;
template <typename T> struct unbox<box<T>> { using type = T; };
unbox<box<int>>::type c4 = 1;

////////////////////////////////////////////////////////////////////////////////
// fact with right evaluation strategy.

template <typename N> struct fact;

template <typename N>
struct fact_impl
    : times<typename fact<typename minus<N, int_<1>>::type>::type, N> {};

template <typename N>
struct fact
    : if_<typename less<N, int_<1>>::type, int_<1>, fact_impl<N>>::type {};
constexpr int c5 = fact<int_<10>>::type::value;

int main(void) { return 0; }

// templatea meta funcotions accepting only nullary metafunctions is easier to
// work with.
