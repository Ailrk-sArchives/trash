#include <iostream>

// what does decay do? //
// Do array decay convertion even when it's passed as reference.
//
// YOU WANT ARRAY DECAY TO POINTER FOR TEMPLATES.

// first note array is not of pointer type, it has it's own type.
// char[4] vs char *.
// You still have c aray decay to pointer, but technically they are different.
//
// Similar situation for a reference to a funcition. you can use a function
// pointer to repace it but they are of different types.
//
// decay<T> is a idenityt mapping except for array types and function types.
// It gives type liek this.
//   decay<char[4]>::type = char *
//   decay<&fn>::type = void(*fn)()

// First see a motivational example //

template <typename T> struct Identity { using type = T; };

template <typename T> T f1(T buff);
template <typename T> Identity<T> f2(T &buff);
void test() {
  const char buf[3] = {0, 0};

  auto x = f1(buf); // pass by value, array get decayed to a pointer.
  auto y = f2(buf); // pass by reference, array is still array.
}

// Why this happens?
// It's from c's heiritage, array cannot be passed by value as a function
// parameter. all arrays passed by value decayed into pointers.
//
// case by case analysis:
//  for f1:
//    buf is of type const char buf[3];
//    buf get pass to T x, by value.
//    compile sees it can't be passed by value becaues it's array type.
//    compile decay it into const char* to fit in.
//
//  for f2:
//    buf is of type const char buf[3];
//    buf pass to T& x as a reference.
//    compile pass the array type as int (&buf)[3].
//
// This also demonstrates that a reference type is really different type from
// the original type.

// now see the standard example:

const char arr[3] = {0, 0};
// this function works for most types.
template <typename T1, typename T2>
inline std::pair<T1, T2> makepair1(T1 x, T2 y) {
  return std::pair<T2, T2>(x, y);
}

// but this one is problematic as if you pass an array type, it will decay //
// if array type const char[3] is T1, it will
// have type return std::pair<const char[3], int>
// you can't construct std::pair<std::string, int>, thus it's ill formed.
template <class T1, class T2>
inline std::pair<T1, T2> make_pair2(T1 &x, T2 &y) {
  return std::pair<T1, T2>(x, y);
}

// this doesn't work. //
// It's because "a" we passed is of type char array and it doesn't decay because
// it's passed by reference.
// auto a1 = make_pair2<std::string, int>(arr, 1);

// What about just settle with the by value version?
// really? copy all the time?

// solution //
// attempt 1. use prefect forwarding
// Why this helps?
template <typename T1, typename T2>
inline std::pair<T1, T2> make_pair3(T1 &&x1, T2 &&x2) {
  return std::pair<T1, T2>(std::forward<T1>(x1), std::forward<T2>(x2));
}

auto a2 = make_pair3<std::string, int>(arr, 1); // no problem.

// with decay //
// finally we get decay. decay force to decay an array type to pointer.
template <typename T1, typename T2>
inline std::pair<std::decay_t<T1>, std::decay_t<T2>> make_pair4(T1 &&x,
                                                                T2 &&y) {
  return std::pair<std::decay_t<T1>, std::decay_t<T2>>(std::forward<T1>(x),
                                                       std::forward<T2>(y));
}
auto a3 = make_pair4<std::string, int>(arr, 1); // no problem.
