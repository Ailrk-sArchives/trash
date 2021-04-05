#include <type_traits>

// http://matt.might.net/articles/lambda-style-anonymous-functions-from-c++-templates/

#include <stdio.h>
#include <stdlib.h>

unsigned int MAXVAR = 0;

template <typename T> struct Expr {

  virtual void set(unsigned int id, void *p) {
    throw "Can't set in a generic expression";
  }

  virtual T eval() { throw "Can't evaluate a generic expression"; }

  virtual Expr<T> *clone() { throw "Can't clone a generic expression"; }
};

template <typename T> class Var : public Expr<T> {
public:
  T *value;

  unsigned int id;

  Var<T>() {
    this->value = nullptr;
    this->id = MAXVAR++;
  }

  Var<T>(const Var<T> &other) {
    this->value = other.value;
    this->id = other.id;
  }

  virtual void set(unsigned int id, void *p) override {
    if (this->id == id) {
      this->value = (T *)p;
    }
  }

  virtual Expr<T> *clone() override { return new Var(*this); }

  virtual T eval() override { return *value; }
};

template <typename T> class Const : public Expr<T> {
private:
  T value;

public:
  Const<T>(T value) { this->value = value; }

  virtual void set(unsigned int id, void *p) override {}

  virtual T eval() { return value; }

  virtual Expr<T> clone() { return new Const<T>(value); }
};

template <typename T> class BinOp : public Expr<T> {
private:
  T (*op)(T, T);
  Expr<T> *lhs;
  Expr<T> *rhs;

public:
  BinOp<T>(T (*op)(T, T), Expr<T> *lhs, Expr<T> *rhs)
      : lhs(lhs), rhs(rhs), op(op) {}

  virtual void set(unsigned int id, void *p) {
    lhs->set(id, p);
    rhs->set(id, p);
  }

  virtual T eval() { return op(lhs->eval(), rhs->eval()); }

  virtual Expr<T> *clone() { return new BinOp(op, lhs, rhs); }
};

int sum(int x, int y) { return x + y; }

int product(int x, int y) { return x * y; }

template <>

struct Expr<int> {
  virtual void set(unsigned int id, void *p) {
    throw "Can't set in a generic (int) expression";
  }

  virtual int eval() { throw "Can't evaluate a generic expression"; }

  virtual Expr<int> *clone() { throw "Can't clone a generic expression"; }

  Expr<int> &operator+(Expr<int> &that) {
    return *(new BinOp(sum, this->clone(), that.clone()));
  }

  Expr<int> &operator*(Expr<int> &that) {
    return *(new BinOp(product, this->clone(), that.clone()));
  }

  Expr<int> &operator+(int &that) {}
};
