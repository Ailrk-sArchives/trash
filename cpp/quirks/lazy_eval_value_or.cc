#include <iostream>
#include <optional>
// https://www.foonathan.net/2017/06/lazy-evaluation/#content

// idea: optional.value_or(xx). IF xx is expensive but we don't need to use it,
//       eager evaluation will waset time on constructing it.
//       We want to create xx by need.

// eager version that will be wasteful in evaluating fallback.
template <typename T, typename U>
T value_or_1(std::optional<T> const &op, U &&fallback) {
  if (op.has_value()) {
    return op.value();
  } else {
    return static_cast<T>(std::forward<U>(fallback));
  }
}

//////////////////////////////////////////////////////////////////////////////
// wrap fallback in a lambda to defer the evaluation
#define VALUE_OR(opt, fallback)                                                \
  [&](const auto &optional) {                                                  \
    if (optional.has_value())                                                  \
      return optional.value();                                                 \
    return static_cast<                                                        \
        typename std::remove_cvref_t<decltype(optional)>::value_type>(         \
        fallback);                                                             \
  }(opt)

void test1() {
  auto result1 = VALUE_OR(std::optional<int>{1}, 2);
  auto result2 = VALUE_OR(std::optional<double>{}, 12);
}

//////////////////////////////////////////////////////////////////////////////
// use sfinae to support both lazy and eager passing.
// instead of passing value, we can pass deferred value.

template <typename T, typename U,
          decltype(static_cast<T>(std::declval<U>())) = 0>
T value_or_2(std::optional<T> const &opt, U &&fallback) {
  if (opt.has_value()) {
    return opt.value();
  }
  return static_cast<T>(std::forward<U>(fallback));
}

template <typename T, typename U,
          decltype(static_cast<T>(std::declval<U>()())) = 0>
T value_or_2(std::optional<T> const &opt, U &&lambda) {
  if (opt.has_value()) {
    return opt.value();
  }
  return static_cast<T>(std::forward<U>(lambda)());
}

// the corresponding suspension creater.
#define LAZY(Expr) [&]() -> decltype((Expr)) { return Expr; }

void test_value_or_2() { auto v = value_or_2(std::optional<int>{1}, LAZY(10)); }
