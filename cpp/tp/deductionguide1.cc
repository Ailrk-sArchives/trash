#include <iostream>

// Conlusion:
// 1. explicit will change the behavior of type deduction.
// 2. when there is explcit constructor, the constructor will not
//    invovle in the copy init resolution
//   side note:
//      1. constructor is also overloaded, and the compiler needs to
//         deduce which constructor is the best fit.
//      2. for explicit constructor, we remove cases of overloading
//         that the constructor can involve into.
//
//         For example. explict constructor won't be considered when
//         the compiler needs to choose the copy constructor for a
//         copy init.
//

// explicit means a constructor cannot be used forf

template <typename T> struct A {
  explicit A(const T &, ...) noexcept;
  A(T &&, ...);
};

int i;

// error, because A is explicit so it's no used for copy initialization.
// thus the compiler will use the second constructor.
// i is deduced as rvalue,
// and we cannot deduce from rvalue reference from #2 because it's a lvalue.
// A a1 = {i, i};

// works, uses #1
A a2{i, i};

// ok, uses #2
A a3{0, i};

// ok, uses #2
A a4{0, i};

// Now let's define some deduction guides.
// notice now to trigger #2 you need to have explicit initialization, so
// the copy init cases above won't work anymore if you use type deduction.
template <typename T> A(const T &, const T &) -> A<T &>;
template <typename T> explicit A(T &&, T &&) -> A<T>;

// this failes now as oppose to a3.
// it's because according to the deduction guide, we
// A a5 = {0, 1};

A a6{1,1};  // #4 best fit, use #2 to initialize.
