#include <iostream>
#include <type_traits>

// let's define some type traits.

// define our version of move
// Now let's take a look at the move
// move take (T &&) universal reference.
// It remove all references first, and add two &&
// which guarantee us to get a rvalue reference.
// steps
// 0. accept universal ref (both lval and rval).
// 1. take type T
// 2. remvoe all reference
// 3. add && to make it universal reference.
// 4. return T with &&, make it a rval.
// Now it's a rval ref, compile can free to call the move
// constructor
template <typename T> typename std::remove_reference<T>::type &&move_(T &&a) {
  return a;
}

template <typename T>
constexpr T &&forward_(typename std::remove_reference<T>::type &&t) noexcept {
  static_assert(!std::is_lvalue_reference<T>::value,
                "template argument substituing T is an lvalue reference type");
  return static_cast<T &&>(t);
}
