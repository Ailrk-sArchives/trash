#include <iostream>
#include <optional>

template <typename T> int foo(std::optional<T> v) { return v.value_or(-1); }

std::optional<int> bar(bool a) {
  if (a) {
    return 1;
  }
  return std::nullopt;
}

template <typename T> constexpr T optchain_(std::optional<T> t) {
  return t.value();
}

template <typename T, typename... Args>
constexpr T optchain_(std::optional<T> t, Args... args) {
  return t.value_or(optchain_(args...));
}

template <typename... Args>
auto optchain(Args... args) -> std::optional<decltype(optchain_(args...))> {
  try {
    auto v = optchain_(args...);
    return v;
  } catch (std::bad_optional_access) {
    return {};
  }
}

void test_optchain() {
  std::cout << "optional chaining" << std::endl;

  auto a = optchain(bar(false), bar(false), std::optional<int>(2),
                    std::optional<int>(3));

  std::cout << a.value() << std::endl;
}

/* template <typename R, typename... Args> */
/* auto optional_chaining(R r, Args... args) { */
/*   return r.value_or(optional_chaining(args...)); */
/* } */

int main(void) {
  auto x = std::optional<int>{1};

  if (auto p = bar(false)) {
    std::cout << p.value() << std::endl;
  } else {
    std::cout << "no nullopt" << std::endl;
  }

  if (auto a = bar(true)) {
    std::cout << a.value() << std::endl;
  } else {
    std::cout << "no val" << std::endl;
  }

  test_optchain();

  return 0;
}
