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

template class Visitor;

class Expr {

  virtual void accept_(visitor_t &visitor) = 0;

public:
  template <typename T>
  T accept(Visitor &visitor) {
    accept_(visitor);
    T result = visitor.result();
    return result;
  }
};

template <typename T> class Lit : public Expr<T> {
public:
  using visitor_t = typename Lit::visitor_t;
  int value;

  Lit(int value) : value(value) {}
  virtual T accept(visitor_t &v) override;
};

template <typename T> class Add : public Expr<T> {
public:
  using visitor_t = typename Add::visitor_t;
  std::unique_ptr<Expr<T>> left, right;

  Add(std::unique_ptr<Expr<T>> left, std::unique_ptr<Expr<T>> right)
      : left(std::move(left)), right(std::move(right)) {}

  virtual T accept(visitor_t &v) override;
};

template <typename T> class Sign : public Expr<T> {
public:
  using visitor_t = typename Sign::visitor_t;
  std::unique_ptr<Expr<T>> expr;

  Sign(std::unique_ptr<Expr<T>> expr) : expr(std::move(expr)) {}

  virtual T accept(visitor_t &v) override;
};

template <typename T> class Visitor {
protected:
  T result_;

public:
  using type = T;
  virtual void visit(Lit<T> &a) = 0;
  virtual void visit(Add<T> &a) = 0;
  virtual void visit(Sign<T> &a) = 0;
  inline virtual type result() { return result_; }
  virtual ~Visitor() {}
};

template <typename T> T Lit<T>::accept(visitor_t &v) { return v.visit(*this); }
template <typename T> T Add<T>::accept(visitor_t &v) { return v.visit(*this); }
template <typename T> T Sign<T>::accept(visitor_t &v) { return v.visit(*this); }

// eval method on all nodes.
class Eval : public Visitor<int> {
public:
  virtual void visit(Lit<int> &a) override { result_ = a.value; }

  virtual void visit(Add<int> &a) override {
    result_ = a.left->accept(*this) + a.right->accept(*this);
  }

  virtual void visit(Sign<int> &a) override {
    auto value = a.expr->accept(*this);
    int res;
    if (value > 0) {
      res = 1;
    } else if (value == 0) {
      res = 0;
    } else {
      res = -1;
    }
    result_ = res;
  }
};

// show method on all nodes.
// this is problematic. Expr<T> used for show is different from that used
// for Eval!.
// We need some other implementation.
class Show : public Visitor<std::string> {
public:
  virtual void visit(Lit<std::string> &a) override {
    result_ = std::to_string(a.value);
  }

  virtual void visit(Add<std::string> &a) override {
    result_ = a.left->accept(*this) + " + " + a.right->accept(*this);
  }

  virtual void visit(Sign<std::string> &a) override {
    std::string value = a.expr->accept(*this);
    auto x = value[0];
    std::string res;
    if (value[0] == '-') {
      res = "-";
    } else {
      res = "+";
    }
    result_ = res;
  }
};

int main(void) {
  std::unique_ptr<Expr<int>> expr =
      std::make_unique<Sign<int>>(std::make_unique<Add<int>>(
          std::make_unique<Add<int>>(std::make_unique<Lit<int>>(7),
                                     std::make_unique<Lit<int>>(2)),
          std::make_unique<Lit<int>>(-10)));

  Eval evaluator{};

  int evaled = expr->accept(evaluator);
  std::cout << "evaluation result:" << evaled << std::endl;

  return 0;
}
