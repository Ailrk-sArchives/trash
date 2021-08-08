#include <iostream>
#include <type_traits>

struct K {
  enum { int_t, float_t } type;

  // if std::is_integral<Integer>::value is true, this specialization pass,
  // thus the default template paramter will be specialize to true.
  template <typename Integer,
            typename std::enable_if<std::is_integral<Integer>::value,
                                    bool>::type = true>
  K(Integer n) : type(int_t) {}

  template <typename Floating,
            typename std::enable_if<std::is_floating_point<Floating>::value,
                                    bool>::type = true>
  K(Floating n) : type(float_t) {}
};

// another way to partial specialize
template <typename T, typename = void> struct A {};

template <typename T>
struct A<T, typename std::enable_if<std::is_floating_point_v<T>>::type> {};

// note:
// A common problem:
// Two overloads that only differ on the default template parameter.
// This cause a problem because the compiler treat them as the same
// specialization, so you end up with redefinition.

int main(void) {
  K n1(1);
  K n2(1.1);

  if (n1.type == K::int_t) {
    std::cout << "K int true" << std::endl;
  }

  if (n2.type == K::float_t) {
    std::cout << "K float true" << std::endl;
  }

  return 0;
}
