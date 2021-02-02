#include <concepts>
#include <iostream>
#include <type_traits>
#include <vector>

// there are different types of constraints.
//
// 1. simple requirement
// 2. nested requirement
// 3. compound requirement
// 4. type requirement

// let's connstrain this function.
// left fold of addition.
template <typename... Args> auto add_(Args &&... args) { return (... + args); }

// what do we want to constrain here?
// 1. + is defined
// 2. typename ...Args are all of the same type
// 3. + should be noexcpet
// 4. return type should be the same

// let's define some handy type traits first. Latter
// we will use them to define the concept

// pattern matching on type list.
template <typename T, typename...> struct first_type { using type = T; };
template <typename... Args>
using first_type_t = typename first_type<Args...>::type;

// template boolean
template <typename T, typename... Ts>
inline constexpr bool are_same_v = std::conjunction_v<std::is_same<T, Ts>...>;

template <typename T, typename... Args>
concept same_as_first_type =
    std::is_same_v<std::remove_cvref_t<T>,
                   std::remove_cvref_t<first_type_t<Args...>>>;

template <typename... Args> requires requires(Args... args) {
  (... + args);                 // 1. simple requirement
  requires are_same_v<Args...>; // 2. nested requirement
  requires sizeof...(Args) > 1; // 3. nested requirement with boolean assertion.
  { (... + args) }
  noexcept->same_as_first_type<Args...>; // 4. compound requirements
}
auto add(Args &&... args) { return (... + args); }

// exercise
template <typename... Args> requires requires(Args... args) {
  (... * args);
  requires are_same_v<Args...>;
  requires sizeof...(args) > 1;
  { (... * args) }
  noexcept->same_as_first_type<Args>;
}
auto mul(Args &&... args) { return (... * args); }

// now you have these constrained functions, but how do you know if they
// are correct constrains or not?
// you need some methods to reliably test your constrains.

template <bool NOEXCEPT, bool hasOperatorPlus, bool validReturnType>
class ObjectMock {

public:
  ObjectMock() = default;

  ObjectMock &operator+(const ObjectMock &rhs) noexcept(NOEXCEPT) requires(
      hasOperatorPlus &&validReturnType) {
    return *this;
  }

  int operator+(const ObjectMock &rhs) noexcept(NOEXCEPT) requires(
      hasOperatorPlus && not validReturnType) {
    return 3;
  }
};

using NoAdd = ObjectMock<true, false, true>;
using ValidClass = ObjectMock<true, true, true>;
using NoNoexcept = ObjectMock<false, true, true>;
using DifferentReturnType = ObjectMock<false, true, false>;

template <typename... Args> concept TestAdd = requires(Args... args) {
  add(args...);
};

static_assert(TestAdd<int, int, int>);
//static_assert(TestAdd<NoAdd, NoAdd>);


int main(void)
{

  return 0;
}
