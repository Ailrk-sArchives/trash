#include <iostream>

// the motivation of move.
//
// First, in C++ you're not allowed to bind rvalue to
// non const referece.
//
// but sometimes you want to, like to steal from whatever
// the reference refers to.
//
// In that case you have rvalue reference now.
// We are allowed to pass a rvalue by rvalue reference, modify
// whatever it has.
// Because it's rvalue, after it's passed we can just forget about
// it.

// reference overload rules:
// 1. rvalue prefer rvalue reference.
// 2. lvalue prefer lvalue reference.
// 3. if there is no rvalue reference overload, rvalue can choose
//    to use const reference overload.
// 4. lvaue can bind to rvalue refernce, but it will prefer lvalue
//    reference first.
// 5. cv qualifier are cosidered after r/l reference.

void incr(int &r) { r++; }

int main() {
  double ss = 1;
  // this is illegal.
  // because when pass ss to incr,
  // there will be an implicit convertion from douhble to
  // int, which will introduce a temporary (rval),
  // which cannot be passed to a non const referece.
  // incr(ss);

  // this works as you convert double& int& to
  incr((int&)(ss));

  std::cout << "value << " << ss << std::endl;
}
