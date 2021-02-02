#include <iostream>
#include <type_traits>
#include <vector>
#include <map>

// Void t and SFINAE
// Void t map sequence of any types to void.
// https://en.cppreference.com/w/cpp/types/void_t
// https://en.cppreference.com/w/cpp/language/sfinae

template <typename T, typename = void> struct is_iterable : std::false_type {};

template <typename T>
struct is_iterable<T, std::void_t<decltype(std::begin(std::declval<T>())),
                                  decltype(std::end(std::declval<T>()))>>
    : std::true_type {};


int main(int argc, char *argv[]) {
  std::cout << is_iterable<std::vector<int>>::value << std::endl;
  std::cout << is_iterable<double>::value << std::endl;
  return 0;
}
