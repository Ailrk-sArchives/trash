#include <functional>
#include <iostream>
#include <type_traits>

template <typename Fn, typename = std::enable_if_t<std::is_convertible<
                           Fn, std::function<int(int)>>::value>>
void template_fn(Fn fn) {
  auto i = fn(1);
  std::cout << "succeed: " << i << std::endl;
}

struct nil {};
template <typename...> struct head;
template <typename T, typename... Ts> struct head<T, Ts...> { using type = T; };
template <> struct head<> { using type = nil; };

head<>::type;

int main(void) {

  auto fn1 = [](int a) { return a + 1; };

  std::cout
      << std::is_convertible<decltype(fn1), std::function<int(int)>>::value
      << std::endl;

  std::cout << std::is_convertible<decltype(fn1), int>::value << std::endl;

  template_fn(fn1);

  return 0;
}
