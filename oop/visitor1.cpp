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

template <typename T> class Visitor;

template <typename T> class Expr {
public:
  using visitor_t = Visitor<T>;
  virtual T visit(visitor_t &visitor) = 0;
};

template <typename T> class Lit : public Expr<T> {
public:
  using visitor_t = typename Lit::visitor_t;
  int value;

  Lit(int value) : value(value) {}
  virtual T visit(visitor_t &v) override;
};

template <typename T> class Add : public Expr<T> {
public:
  using visitor_t = typename Add::visitor_t;
  std::unique_ptr<Expr<T>> left, right;

  Add(std::unique_ptr<Expr<T>> left, std::unique_ptr<Expr<T>> right)
      : left(std::move(left)), right(std::move(right)) {}

  virtual T visit(visitor_t &v) override;
};

template <typename T> class Visitor {
public:
  using type = T;
  virtual type lit(Lit<T> &a) = 0;
  virtual type add(int a, int b) = 0;
  virtual type abs(Lit<int> &a) = 0;
  virtual ~Visitor() {}
};

template <typename T> T Lit<T>::visit(visitor_t &v) { return v.lit(*this); }

template <typename T> T Add<T>::visit(visitor_t &v) {
  return v.add(left->visit(v), right->visit(v));
}

class Eval : public Visitor<int> {
public:
  virtual type lit(Lit<int> &a) override { return a.value; }

  virtual type add(int a, int b) override { return a + b; }

  virtual type abs(Lit<int> &a) override {
    if (a.value < 0) {
      return -a.value;
    } else {
      return a.value;
    }
  }
};

int main(void) {
  std::unique_ptr<Expr<int>> expr = std::make_unique<Add<int>>(
      std::make_unique<Lit<int>>(1), std::make_unique<Lit<int>>(2));

  Eval evaluator{};

  int v = expr->visit(evaluator);
  std::cout << "result:" << v << std::endl;

  return 0;
}
