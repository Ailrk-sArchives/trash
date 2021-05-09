#include <iostream>
#include <memory>

// solve the expression problem with visitor pattern.
// Subtyping is easy to extend new types, but hard to add new
// function that works for every types.
//
// Visitor allows you to extend function easiler.
//
// C++ doesn't allows class member template to be virtual.
// it can be done but it's not there.
// This make things a bit harder for using visitors.

// Features:
// 1. Everything are templated. So visitors can return different types
// 2. visit is a virtual function. You can have subclass assign to Expr
//    and still invoke the right method.
// 3. visitor contains heterougenous functions. (not everything visit)

class VisitorBase {};

template <typename T> class Visitor : public VisitorBase {
protected:
  T result_;

public:
  T result() { return result_; }
  ~Visitor() {}
};

class Expr {
  virtual void accept_do(VisitorBase &v) = 0;

public:
  template <typename T> T accept(Visitor<T> &v) {
    accept_do(v);
    return v.result();
  }
};

class Lit : public Expr {
  virtual void accept_do(VisitorBase &v) override;

public:
  int value;
};

class Add : public Expr {
  virtual void accept_do(VisitorBase &v) override;

public:
  std::unique_ptr<Expr> left, right;

  Add(std::unique_ptr<Expr> left, std::unique_ptr<Expr> right)
      : left(std::move(left)), right(std::move(right)) {}
};


// visitor definee here.

int main(void) {
  // auto e = std::make_unique<Lit>(1);

  // Visitor<int> visitor{};

  // int a = e->accept(visitor);

  return 0;
}
