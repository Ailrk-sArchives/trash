#include <iostream>
#include <type_traits>

#define TESTING

/* utils */
template <typename T> struct identity { using type = T; };
template <typename T> struct tag { using type = typename T::tag; };
template <typename F, typename... Ts> struct apply {
  using type = typename F::template apply<Ts...>::type;
};

/* lazy eval */
template <typename T> struct already_lazy;
template <typename Expr> struct lazy : Expr {};
template <typename T> struct lazy<already_lazy<T>> { using type = T; };
template <template <typename...> typename F, typename... Ts>
struct lazy<F<Ts...>> : F<typename lazy<Ts>::type...> {};

/* conditional */
template <typename C, typename T, typename F> struct if_ {
private:
  template <bool C_, typename T_, typename F_> struct if_impl;
  template <typename T_, typename F_> struct if_impl<true, T_, F_> : T {};
  template <typename T_, typename F_> struct if_impl<false, T_, F_> : F {};

public:
  using type = typename if_impl<C::type::value, T, F>::type;
};

/*
 * values
 * value is defined by it's constructor and a tag to indicate it's type.
 * all values are boxed and behaves like nullary metafunction.
 * */
///! int type
struct int_tag {}; // this is really the type.
template <int N> struct int_ : identity<int_<N>> {
  using tag = int_tag;
  static const int value = N;
};

///! char type
struct char_tag {}; // this is really the type.
template <char N> struct char_ : identity<char_<N>> {
  using tag = char_tag;
  static const char value = N;
};

///! bool type
struct bool_tag {}; // this is really the type.
template <bool N> struct bool_ : identity<bool_<N>> {
  using tag = bool_tag;
  static const bool value = N;
};

///! logic not
template <typename T> struct not_;
template <> struct not_<bool_<true>> : bool_<false> {};
template <> struct not_<bool_<false>> : bool_<true> {};
///! logic and
template <typename... Ts>
struct and_ : bool_<(Ts::type::value && ... && true)> {};
///! logic or
template <typename... Ts>
struct or_ : bool_<(Ts::type::value || ... || false)> {};

#ifdef TESTING
static_assert(and_<bool_<true>, bool_<true>>::type::value);
static_assert(or_<bool_<false>, bool_<true>>::type::value);
#endif

///! maybe type
struct maybe_tag {};
template <typename T> struct just : identity<just<T>> {
  using tag = maybe_tag;
};
struct nothing : identity<nothing> {
  using tag = maybe_tag;
};

///! from from just
template <typename T> struct from_just;
template <typename T> struct from_just<just<T>> : just<T>::type {};

/* type predicates */

///! equal
template <typename A, typename B> struct eql : bool_<false> {};
template <typename A> struct eql<A, A> : bool_<true> {};

///! is integral type
template <typename A>
struct is_integral : or_<eql<A, int_tag>,  //
                         eql<A, bool_tag>, //
                         eql<A, char_tag>> {};

#ifdef TESTING
static_assert(eql<int_<10>, int_<10>>::type::value);
static_assert(eql<int_tag, int_tag>::type::value);
static_assert(is_integral<int_tag>::value);
#endif

/* typeclasses */

///! class num
template <typename T> struct num;
template <> struct num<int_tag> {
  struct plus : identity<plus> {
    template <typename A, typename B>
    using apply = int_<A::type::value + B::type::value>;
  };
  struct minus : identity<minus> {
    template <typename A, typename B>
    using apply = int_<A::type::value - B::type::value>;
  };
  struct times : identity<times> {
    template <typename A, typename B>
    using apply = int_<A::type::value * B::type::value>;
  };
};

///! class: eq
///!   equal<A, B> -> bool_tag
///!   not_equal<A, B> -> bool_tag
template <typename T> struct eq;
template <typename T> struct eq_defaults {
  struct not_equal : identity<not_equal> {
    template <typename A, typename B>
    struct apply : not_<typename ::apply<typename eq<T>::equal, A, B>::type> {};
  };
};

template <> struct eq<int_tag> : eq_defaults<int_tag> {
private:
  template <typename A, typename B> struct equal_to {
    using type = bool_<A::type::value == B::type::value>;
  };

public:
  struct equal : identity<equal> {
    template <typename A, typename B> struct apply : equal_to<A, B> {};
  };
};

template <> struct eq<maybe_tag> : eq_defaults<maybe_tag> {
private:
  template <typename A, typename B>
  struct equal_just_impl : identity<equal_just_impl<A, B>> {
    using type = bool_<false>;
  };
  template <> struct equal_just_impl<nothing, nothing> {
    using type = bool_<true>;
  };
  template <typename A> struct equal_just_impl<just<A>, just<A>> {
    using type = bool_<true>;
  };

public:
  struct equal : identity<equal> {
    template <typename A, typename B>
    struct apply : equal_just_impl<A, B>::type {};
  };
};

#ifdef TESTING
static_assert(eq<int_tag>::not_equal::apply<int_<20>, int_<10>>::type::value);
static_assert(eq<int_tag>::equal::apply<bool_<true>, bool_<true>>::type::value);
static_assert(eq<maybe_tag>::equal::apply<nothing, nothing>::type::value);
static_assert(
    !eq<maybe_tag>::equal::apply<just<int_<1>>, just<int_<2>>>::type::value);
#endif

///! class ord
///!    less<A, B> -> bool_tag
///!    less_equal<A, B> -> bool_tag
///!    greater<A, B> -> bool_tag
///!    greater_equal<A, B> -> bool_tag
template <typename T> struct ord;
template <typename T> struct ord_defaults {
  struct less : identity<less> {
    template <typename A, typename B>
    using apply =
        and_<typename ::apply<typename ord<T>::less_equal, A, B>::type, //
             typename not_<
                 typename ::apply<typename eq<T>::equal, A, B>::type>::type //
             >;
  };

  struct greater : identity<greater> {
    template <typename A, typename B>
    using apply =
        not_<typename ::apply<typename ord<T>::less_equal, A, B>::type>;
  };

  struct greater_equal : identity<greater_equal> {
    template <typename A, typename B>
    using apply = not_<typename ::apply<typename ord<T>::less, A, B>::type>;
  };
};

template <> struct ord<int_tag> : ord_defaults<int_tag> {
  struct less_equal : identity<less_equal> {
    template <typename A, typename B>
    using apply = bool_<(A::type::value <= B::type::value)>;
  };
};

template <> struct ord<bool_tag> : ord_defaults<bool_tag> {
  struct less_equal : identity<less_equal> {
    template <typename A, typename B>
    using apply = bool_<(A::type::value <= B::type::value)>;
  };
};

#ifdef TESTING
static_assert(
    ord<int_tag>::greater_equal::apply<int_<10>, int_<10>>::type::value);
static_assert(
    ord<int_tag>::greater_equal::apply<int_<10>, int_<5>>::type::value);
static_assert(ord<int_tag>::greater::apply<int_<10>, int_<5>>::type::value);
#endif

/* quotes and variable */
template <typename T> struct quote { using type = T; };
template <typename T> struct unquote;
template <typename T> struct unquote<quote<T>> : T {};

template <typename Id> struct var : identity<var<Id>> {};

#ifdef TESTING
static_assert(
    eql<                                                                    //
        int_<30>,                                                           //
        unquote<quote<num<int_tag>::plus::apply<int_<10>, int_<20>>>>::type //
        >::type::value);
struct xvar1_ {};
using xvar1 = var<xvar1_>;
using quote_with_variable = quote<eq<int_tag>::equal::apply<int_<10>, xvar1>>;
#endif

/*
 * let binding
 * let is implemented by substituting var<T> with it's corresponding
 * quote. On evaluation we evaluate the quote.
 * */

template <typename A, typename E, typename In> struct let_impl {
  using type = In;
};

///! if in clause is A itself, just forward the expression.
template <typename A, typename E, typename In>
struct let_in_syntax : let_impl<A, E, In> {};
template <typename A, typename E> struct let_in_syntax<A, E, A> {
  using type = E;
};

///! unrap E and In.
template <typename A, typename E, typename In> struct strict_let_;
template <typename A, typename E, typename In>
struct strict_let_<A,        //
                   quote<E>, //
                   quote<In>> : quote<typename let_in_syntax<A, E, In>::type> {
};

///! force evaluate quotes passed from let_
template <typename A, typename E, typename In>
struct let_ : strict_let_<          //
                  typename A::type, //
                  typename E::type, //
                  typename In::type> {};

///! quote expressions to avoid accidental evaluation.
template <typename A, typename E> struct let {
  template <typename In> using in = let_<A, quote<E>, quote<In>>;
};

#ifdef TESTING
// struct x_ {};
// using x = var<x_>;
// let<x, num<int_tag>::plus::apply<int_<10>, int_<20>> //
//     >::in<num<int_tag>::plus::apply<int_<20>, x>     //
//           >::type;
#endif

#ifdef TESTING
// struct x1_;
// using x1 = var<x1_>;
// quote<num<int_tag>::plus::apply<int_<10>, x1>>;
#endif

/* helper */
template <typename T>
struct reflexive : eq<typename tag<T>::type>::equal::template apply<T, T> {};

/* test */

namespace {
static_assert(reflexive<int_<10>>::type::value);
} // namespace

// first class angle bracket expression
