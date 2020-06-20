#include <iostream>

// value space, type space, SFINAE space.

template <typename Ty, Ty V> struct integral_constant {
  static constexpr Ty value = V;
};

template <bool B> using bool_constant = integral_constant<bool, B>;

using true_type = bool_constant<true>;
using false_type = bool_constant<false>;

// partial specialization
// some type traits with pattern matching.
template <typename T> struct is_reference : false_type {};
template <typename T> struct is_reference<T &> : true_type {};
template <typename T> struct is_reference<T &&> : true_type {};
template <typename T>
inline constexpr bool is_reference_v = is_reference<T>::value;

// e.g
static_assert(is_reference_v<int &>);
static_assert(!is_reference_v<int>);

template <typename T> struct remove_reference { using type = T; };
template <typename T> struct remove_reference<T &> { using type = T; };
template <typename T> struct remove_reference<T &&> { using type = T; };
template <typename T>
using remove_reference_t = typename remove_reference<T>::type;

static_assert(std::is_same<remove_reference_t<int &&>, int>::value);
static_assert(std::is_same<remove_reference_t<int &>, int>::value);
static_assert(std::is_same<remove_reference_t<int>, int>::value);

namespace Illformed {
// add lvalue reference
// problem, cannot add ref to void. (ill formed code.)
template <typename T> struct add_lvalue_reference { using type = T &; };
template <typename T>
using add_lvalue_reference_t = typename add_lvalue_reference<T>::type;

static_assert(std::is_same<add_lvalue_reference_t<int &&>, int &>::value);
static_assert(std::is_same<add_lvalue_reference_t<int &>, int &>::value);
static_assert(std::is_same<add_lvalue_reference_t<int>, int &>::value);
// this doesn't work.
// static_assert(std::is_same<add_lvalue_reference_t<void>, void>::value);

} // namespace Illformed

// SFINAE
// always prefer partial specialization when pattern matching.
namespace Ok1 {
template <typename T, typename Enable> struct ALR_impl { // base tp
  using type = T;
};

template <typename T>
struct ALR_impl<T, remove_reference_t<T &>> { // partial specialization
  using type = T &;
};

template <typename T> // call site
struct add_lvalue_reference : ALR_impl<T, remove_reference_t<T>> {};
} // namespace Ok1

// using void_t to match void.
namespace Ok2 {
template <typename...> using void_t = void;

// mass produce type traists.
template <typename T, typename> struct ALR_impl { using type = T; };
template <typename T> struct ALR_impl<T, void_t<T &>> { using type = T &; };
template <typename T> struct ALR_impl<T, void_t<T &&>> { using type = T &&; };
template <typename T> struct ALR_impl<T, void_t<T *>> { using type = T *; };

template <typename T> struct add_lvalue_reference : ALR_impl<T, void> {};
template <typename T> struct add_rvalue_reference : ALR_impl<T, void> {};
template <typename T> struct add_pointer : ALR_impl<T, void> {};

// declval fucntion sig (expression sfinae)
template <typename T> auto declval() noexcept -> add_rvalue_reference<T>;
template <typename T, typename U>
using assignment_result_t = decltype(declval<T>() = declval<U>());

} // namespace Ok2
