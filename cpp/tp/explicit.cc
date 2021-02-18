#include <iostream>

// a constructor is explicit means it cannot be used for
//   1. implict conversion.
//   2. copy initialization.
// NOTE: if a constructor is not explicit, it's converting constructor.
//
// NOTE:
//      1. constructor is also overloaded, and the compiler needs to
//         deduce which constructor is the best fit.
//      2. for explicit constructor, we remove cases of overloading
//         that the constructor can involve into.
//
//         For example. explict constructor won't be considered when
//         the compiler needs to choose the copy constructor for a
//         copy init.
//
//         Further, explicit won't use explicit for convertion.
//         By conversion I mean (B)1. if the constructor is
//         explicit B::B(int), this won't work.
//
//      3. Consider explicit as a predicate that limit the visibility of
//         the type constructor.
//         Normally a converting constructor is visible in all cases,
//         including copy init and conversion.
//         But explict type constructor are hidden for those cases.
//
// SIDENOTE:
//  I feel most of confusion from C++ comes for different types of overloading,
//  there are so many of them that can happen in so many different cases.
//
//  deduction guide allows you to overload type.
//  function overloading allows you to overload function invocation,
//  overloading on constructor determines what type you end up with.
//
//  And there are some sporadic predicates like explicit in the language that
//  somehow alter the overloading behavior.
//
//  The main abstraction in c++ is adhoc polymorphism.  Parametric polymorphism is
//  also overloadble, which makes things complicated.

// let's do a little experiment.
struct A {
  A(int) {}
  A(int, int) {}
  operator bool() const { return true; }
};

struct B {
  explicit B(int) {}
  explicit B(int, int) {}
  explicit operator bool() const { return true; }
};

int main(void) {
  A a1 = 1;
  A a2(2);
  A a3{4, 5};
  A a4 = {4, 5};
  A a5 = (A)1;
  if (a1)
    ;

  // B b1 = 1; all explicit constructors are not considered for copy init.
  B b2(2);

  // this works because
  B b3 = (B)1;

  return 0;
}
