#include <iostream>

// solve the expression problem with visitor pattern.
// Subtyping is easy to extend new types, but hard to add new
// function that works for every types.
//
// Visitor allows you to extend function easiler.
//
// C++ doesn't allows class member template to be virtual.
// it can be done but it's not there.
// This make things a bit harder for using visitors.

template <typename T> class Lit;
template <typename T> class Add;

template <typename T> class Visitor {
public:
  virtual T lit(Lit<T> &a) = 0;
  virtual T add(Add<T> &a) = 0;
  virtual ~Visitor() {}
};

class Expr {
  template <typename T> T visit(Visitor<T> &visitor);
};


