#include <iostream>

// =======================================================
// sfinae. Things can either be well formed or ill formed.

// template that holds a value and make it a type
template <typename T, T v> struct integral_const_ {
  static constexpr T value = v;
};

// type family indexed by true and false.
template <bool B> using bool_const_ = integral_const_<bool, B>;
using true_type_ = bool_const_<true>;
using false_type_ = bool_const_<false>;

// type family index by nat
template <size_t S> using nat_const_ = integral_const_<size_t, S>;
using nat_1_type_ = nat_const_<1>;
using nat_2_type_ = nat_const_<2>;
using nat_3_type_ = nat_const_<3>;
using nat_4_type_ = nat_const_<4>;
using nat_5_type_ = nat_const_<5>;

// =======================================================
// partial specialization
// resemble pattern matching
template <typename T> struct is_reference_ : false_type_ {};
template <typename T> struct is_reference_<T &> : true_type_ {};
template <typename T> struct is_reference_<T &&> : true_type_ {};

static_assert(is_reference_<int &>::value);
static_assert(!is_reference_<int>::value);

// we can act on tope of types
template <typename T> struct remove_reference_ { using type = T; };
template <typename T> struct remove_reference_<T &> { using type = T; };
template <typename T> struct remove_reference_<T &&> { using type = T; };

static_assert(std::is_same_v<remove_reference_<int &&>::type,
                             remove_reference_<int>::type>);

// =======================================================
// add reference
// use partial specialization to avoid al void cases.
template <typename T> struct add_lvalue_reference_bad_ { using type = T &; };
template <> struct add_lvalue_reference_bad_<void> { using type = void; };
template <> struct add_lvalue_reference_bad_<const void> { using type = void; };
template <> struct add_lvalue_reference_bad_<volatile void> {
  using type = void;
};
template <> struct add_lvalue_reference_bad_<const volatile void> {
  using type = void;
};

static_assert(std::is_same_v<add_lvalue_reference_bad_<int>::type, int &>);
static_assert(std::is_same_v<add_lvalue_reference_bad_<void>::type, void>);

// =======================================================
// Use sfinae to ask the compiler if certain specialization
// will be well formed or not.
// We do this to avoid void.
// This works because for all types other than void,
// remove_reference_<T> should be the same things a
// remove_reference_<T&>

template <typename T, typename Enable> struct ALR_impl1 { using type = T; };

template <typename T> struct ALR_impl1<T, std::remove_reference_t<T &>> {
  using type = T &;
};

template <typename T>
struct add_lvalue_reference_1_ : ALR_impl1<T, std::remove_reference_t<T>> {};

static_assert(std::is_same_v<add_lvalue_reference_1_<int>::type, int &>);
static_assert(std::is_same_v<add_lvalue_reference_1_<void>::type, void>);

// =======================================================
// void t map any type to void
template <typename...> using void_t = void;

// base case.
template <typename T, typename Enable> struct ALR_impl2 { using type = T; };
// try this one first
template <typename T> struct ALR_impl2<T, void_t<T &>> { using type = T &; };

template <typename T> struct add_lvalue_reference_2_ : ALR_impl2<T, void> {};

static_assert(std::is_same_v<add_lvalue_reference_2_<int>::type, int &>);

// this one first attempt ALR_impl2<T, void_t<T &>>
// and get ALR_impl2<void, void_t<void &>>, which is ill formed.
// Thus fallback to base case ALR_impl2<T, Enable>, and becomes
// ALR_impl2<void, void>::type = void.
static_assert(std::is_same_v<add_lvalue_reference_2_<void>::type, void>);

// =======================================================
// more on void_t
// if it's void batch the base case.
template <typename T, typename = void> struct has_foo : std::false_type {};
// if T has .foo, specialize this one.
template <typename T>
struct has_foo<T, decltype(std::declval<T &>().foo())> : std::true_type {};

// =======================================================
// declval
// for unevaluated expression. We need to create a value to get the
// type, but we don't need the value itself.
template <typename T>
auto declval_() noexcept -> std::add_rvalue_reference_t<T>;

template <typename T, typename U>
using assignment_result_t = decltype(declval_<T>() = declval_<U>());

static_assert(std::is_same_v<assignment_result_t<int &, double>, int &>);
static_assert(std::is_same_v<assignment_result_t<char &, double>, char &>);

// this is ill formed
static_assert(!std::is_same_v<assignment_result_t<char &, double>, char *>);

// =======================================================
// expression sfinae
// evaluate the wellformness of the expression, and project it to
// the type space.
template <typename T, typename U, typename Enable>
struct is_assignable_impl : std::false_type {};
template <typename T, typename U>
struct is_assignable_impl<T, U, decltype(void(declval_<T>() = declval_<U>()))>
    : std::true_type {};

template <typename T, typename U>
struct is_assignable_ : is_assignable_impl<T, U, void> {};

// you can't assign int to int&, hmmm
static_assert(is_assignable_<int &, int>::value);

// =======================================================
// let's make some other type traits
// a class is polymorphic if we can dynamic cast it so...
template <typename T, typename> struct IP_impl : std::false_type {};
template <typename T>
struct IP_impl<T, decltype(dynamic_cast<void *>(
                      std::declval<std::remove_cv_t<T> *>()))>
    : std::true_type {};

template <typename T> struct is_polymorphic_ : IP_impl<T, void *> {};

// constructable
template <typename T, typename...> struct IC_impl : std::false_type {};
template <typename T, typename... Ts>
struct IC_impl<
    T, decltype(void(::new (std::declval<void *>()) T(std::declval<Ts>()...))),
    Ts...> : std::true_type {};

template <typename T, typename... Ts>
struct is_constructible_ : IC_impl<T, void, Ts...> {};
