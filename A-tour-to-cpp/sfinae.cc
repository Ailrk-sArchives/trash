#include <iostream>

// value space, type space, SFINAE space.

template <typename Ty, Ty V>
struct integral_constant {
  static constexpr Ty value = V;
};

template <bool B>
using bool_constant = integral_constant<bool, B>;

using true_type = bool_constant<true>;
using false_type = bool_constant<false>;

// partial specialization
// some type traits with pattern matching.
template <typename T> struct is_reference       : false_type {};
template <typename T> struct is_reference<T&>   : true_type {};
template <typename T> struct is_reference<T&&>  : true_type {};
template <typename T> inline constexpr bool is_reference_v = is_reference<T>::value;

// e.g
static_assert(is_reference_v<int&>);
static_assert(!is_reference_v<int>);

template <typename T> struct remove_reference      { using type = T; };
template <typename T> struct remove_reference<T&>  { using type = T; };
template <typename T> struct remove_reference<T&&> { using type = T; };
template <typename T> using remove_reference_t = typename remove_reference<T>::type;

static_assert(std::is_same<remove_reference_t<int&&>, int>::value);
static_assert(std::is_same<remove_reference_t<int&>, int>::value);
static_assert(std::is_same<remove_reference_t<int>, int>::value);

// add lvalue reference
template <typename T> struct add_lvalue_reference { using type = T&; };
template <typename T> using add_lvalue_reference_t = typename add_lvalue_reference<T>::type;

static_assert(std::is_same<add_lvalue_reference_t<int&&>, int&>::value);
static_assert(std::is_same<add_lvalue_reference_t<int&>, int&>::value);
static_assert(std::is_same<add_lvalue_reference_t<int>, int&>::value);

