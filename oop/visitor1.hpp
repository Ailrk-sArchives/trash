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

template <typename T> class Visitor;

template <typename T, typename Visitor = Visitor<T>> class Expr {
public:
  virtual T visit(Visitor &visitor) = 0;
};

template <typename T, typename Visitor = Visitor<T>>
class Lit : public Expr<T> {
public:
  int vlaue;
  virtual T visit(Visitor &v) override;
};

template <typename T, typename Visitor = Visitor<T>>
class Add : public Expr<T> {
public:
  Expr<T> left, right;
  virtual T visit(Visitor &v) override;
};

template <typename T> class Visitor {
public:
  using type = T;
  virtual T lit(Lit<T> &a) = 0;
  virtual T add(Add<T> &a) = 0;
  virtual ~Visitor() {}
};

class K {
public:
  void foo();
};

template <typename T> T Lit<T>::visit(Visitor<T> &v) { return v.lit(*this); }

int main(void) { return 0; }
