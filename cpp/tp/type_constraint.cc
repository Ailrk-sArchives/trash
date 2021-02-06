#include <iostream>
#include <type_traits>

// decltype returns the type of expression passed in.
// declval allows you to pass a type and as if get an
// instance of the type.

struct has_get_date_iml {

  // test is the function give us answer.
  // if the type T passed in has get_date, the first version will
  // get specialized, returh value of test will be true type.
  // But if the type doesn't have the get_date method, the second
  // test will take all other cases and return false type.
  template <typename T,
            typename get_date = decltype(std::declval<const T &>().get_date())>
  static std::true_type test(int);

  // match on anything else and ignore it.
  template <typename...> static std::false_type test(...);
};

// this is a helper function, all it does is to
// have a stand alone type based on behavior of test.
template <typename T>
struct has_get_date : decltype(has_get_date_iml::test<T>(0)) {};

struct GetDate {
  int get_date() { return 1; }
};

int main(void) {
  GetDate a;

  std::cout << has_get_date<decltype(a)>::value << std::endl;
  return 0;
}
