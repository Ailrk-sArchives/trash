#include <assert.h>
#include <cstddef>
#include <iostream>
#include <memory>
#include <regex>
#include <string>
#include <thread>
#include <unordered_map>
#include <vector>

class Complex {
  double re, im;

public:
  Complex(double r, double i) : re{r}, im{i} {}
  Complex(double r) : re{r}, im{0} {}
  Complex() : re{0}, im{0} {}
  double real() const { return re; }
  void real(double d) { re = d; }
  double imag() const { return im; }
  void imag(double i) { im = i; }

  Complex &operator+=(Complex z) {
    re += z.re;
    im += z.im;
    return *this;
  }

  Complex &operator-=(Complex z) {
    re -= z.re;
    im -= z.im;
    return *this;
  }

  Complex &operator*=(Complex);
  Complex &operator/=(Complex);
};

Complex &Complex::operator*=(Complex z) {
  re = re * z.re - im * z.im;
  im = re * z.re + im * z.im;
  return *this;
}

class Vector {
public:
  Vector(int s) : sz{s}, elem(new double[s]), current{elem} {
    if (s < 0) {
      throw std::length_error("Vector constructor: negative size");
    }
    for (int i = 0; i != s; ++i)
      elem[i] = 0;
    current++;
  }

  Vector(std::initializer_list<double> list)
      : sz(static_cast<int>(list.size())), elem(new double[sz]), current(elem) {
    std::copy(list.begin(), list.end(), elem);
  };

  ~Vector() { delete[] elem; }

  int size() const;
  void push_back(double);
  Vector read(std::istream &is);

  double &operator[](int i);

private:
  int sz;
  double *elem;
  double *current;
};

int Vector::size() const { return sz; }

double &Vector::operator[](int i) {
  assert(i < sz && i > 0);
  return elem[i];
}

void Vector::push_back(double val) {
  try {
    current += 1;
    *current = val;
  } catch (std::out_of_range &err) {
    std::cout << err.what() << std::endl;
  }
}

class Container {
public:
  virtual double &operator[](int) = 0;
  virtual int size() const = 0;
  virtual ~Container() {}
};

class Vec : public Container {
public:
  Vec(int s) : v(s) {}
  ~Vec() {}
  double &operator[](int i) override { return v[i]; }
  int size() const override { return v.size(); }

private:
  Vector v;
};

struct Point {
  int x;
  int y;
};

// class hierachies.
class Shape {
public:
  virtual Point center() const = 0;
  virtual void move(Point to) = 0;
  virtual void draw() const = 0;
  virtual void rotate(int angle) = 0;
  virtual ~Shape() {}
};

class Circle : public Shape {
public:
  Circle(Point p, int rad);
  Point center() const override { return x; }

  void move(Point to) override { x = to; }

  void draw() const override;
  void rotate(int) override {}

private:
  Point x;
  int r;
};

class Smiley : public Circle {
public:
  Smiley(Point p, int rad) : Circle(p, rad), mouth(nullptr) {}
  ~Smiley() {
    delete mouth;
    for (auto p : eyes) {
      delete p;
    }
  }

  void move(Point p) override;
  void draw() const override;
  void rotate(int) override;
  void set_mouth(Shape *s);
  virtual void wink(int i);

private:
  std::vector<Shape *> eyes;
  Shape *mouth;
};

void Smiley::draw() const {
  Circle::draw();
  for (auto p : eyes) {
    p->draw();
  }
  mouth->draw();
}

enum class Kind { CIRCLE, TRIANGLE, SMILEY };

std::unique_ptr<Shape> read_shape(std::istream is, Kind k) {

  if (k == Kind::CIRCLE) {
    Point p = {.x = 10, .y = 20};
    int r = 20;
    return std::unique_ptr<Shape>(new Circle(p, r));
  }
  return nullptr;
}

class X {
public:
  X(int);                  // og
  X();                     // default
  X(const X &);            // copy
  X(X &&);                 // move
  X &operator=(const X &); // move assign
  X &operator=(X &&);      // copy assign
  ~X();                    // destructor.
};

// resource management

class Some {

public:
  Some() {
    std::thread t;
    my_threads.push_back(std::move(t));
  }

private:
  std::vector<std::thread> my_threads;
};

template <typename T> class V {
private:
  T *elem;
  int sz;

public:
  explicit V(int s);
  explicit V(std::initializer_list<T> list)
      : sz(static_cast<int>(list.size())) {
    elem = new T[sz];
    std::copy(list.begin(), list.end(), elem);
  };

  ~V() { delete[] elem; }
  T &operator[](int i);
  const T &operator[](int i) const;
  int size() const { return sz; }
};

template <typename T> V<T>::V(int s) {
  if (s < 0)
    throw std::range_error("negative size");

  elem = new T[s];
  sz = s;
}

template <typename T> const T &V<T>::operator[](int i) const {
  if (i < 0 || size() <= i)
    throw std::out_of_range("V::operator[]");
  return elem[i];
}

template <typename T> struct SpaceVec {
  T x;
  T y;
  T z;
};

// variable template;
template <typename T> constexpr T viscosity = 0.4;

template <typename T> constexpr SpaceVec<T> external_acc{T{}, T{-9.8}, T{}};

auto vis2 = 2 * viscosity<double>;
auto acc = external_acc<float>;

// currying in type
template <typename Value>
using StringMap = std::unordered_map<std::string, Value>;

// aliase. all std containers has ::value_type
template <typename C> using Value_type = typename C::value_type;

template <typename Container> void algo(Container &c) {
  V<Value_type<Container>> vec;
}

// compile time if
template <typename T> void update(T &target) {
  if constexpr (std::is_pod<T>::value) {
    // do something
  }
}

// fold expresions
template <typename... T> int sum(T... v) { return (v + ... + 0); }

// string
void dostring() {
  std::string name = "some literal";
  std::string address{"Fake address st."};
  std::string s = name.substr(3, 5);
  name.replace(0, 3, "no");
  name[0] = toupper(name[0]);
  printf("std::string to cstr: %s\n", name.c_str());
}

// string view (ptr length pair) like slice in rust.
std::string cat(std::string_view sv1, std::string_view sv2) {
  std::string res(sv1.length() + sv2.length(), 0);
  char *p = &res[0];
  std::copy(sv2.begin(), sv2.end(), p);
  return res;
}

// iter over while matching with regex
// sregex_iterator = string regex iterator.
void regex_iter() {
  std::string input = "aa as; asd ++e^asdf asdfg";
  std::regex pat(R"(\s+(\w+))");
  for (std::sregex_iterator p(input.begin(), input.end(), pat);
       p != std::sregex_iterator(); ++p) {
    std::cout << (*p)[1] << std::endl;
  }
}

// iostream
// vals  -> ostream -> stream buffer -> somewhere.
// vals  <- ostream <- stream buffer <- somewhere.




