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

template <typename T> class VisitorBase;

template <typename T> class Expr {
public:
  using visitor_t = VisitorBase<T>;
  virtual T accept(visitor_t &visitor) = 0;
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

template <typename T> class VisitorBase {
public:
  using type = T;
  virtual type visit(Lit<T> &a) = 0;
  virtual type visit(Add<T> &a) = 0;
  virtual type visit(Sign<T> &a) = 0;
  virtual ~VisitorBase() {}
};

template <typename T> T Lit<T>::accept(visitor_t &v) { return v.visit(*this); }
template <typename T> T Add<T>::accept(visitor_t &v) { return v.visit(*this); }
template <typename T> T Sign<T>::accept(visitor_t &v) { return v.visit(*this); }

// eval method on all nodes.
class Eval : public VisitorBase<int> {
public:
  virtual type visit(Lit<int> &a) override { return a.value; }

  virtual type visit(Add<int> &a) override {
    return a.left->accept(*this) + a.right->accept(*this);
  }

  virtual type visit(Sign<int> &a) override {
    auto value = a.expr->accept(*this);
    if (value > 0) {
      return 1;
    } else if (value == 0) {
      return 0;
    } else {
      return -1;
    }
  }
};

// show method on all nodes.
// this is problematic. Expr<T> used for show is different from that used
// for Eval!.
// We need some other implementation.
class Show : public VisitorBase<std::string> {
public:
  virtual type visit(Lit<std::string> &a) override {
    return std::to_string(a.value);
  }

  virtual type visit(Add<std::string> &a) override {
    return a.left->accept(*this) + " + " + a.right->accept(*this);
  }

  virtual type visit(Sign<std::string> &a) override {
    std::string value = a.expr->accept(*this);
    auto x = value[0];
    if (value[0] == '-') {
      return "-";
    }
    return "+";
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
