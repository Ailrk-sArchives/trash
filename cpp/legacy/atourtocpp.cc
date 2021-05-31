#include <assert.h>
#include <cstddef>
#include <execution>
#include <forward_list>
#include <fstream>
#include <iostream>
#include <list>
#include <memory>
#include <mutex>
#include <regex>
#include <set>
#include <string>
#include <thread>
#include <unordered_map>
#include <variant>
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

// containers
struct Entry {
  std::string name;
  unsigned int number;
  bool operator<(const Entry &other);
};

std::vector<Entry> phone_book = { // using initializer_list
    {"David Hume", 123456},
    {"Karl Popper", 234567},
    {"Bertrand Arthur", 345678}};

auto movev(std::vector<Entry> &&en) -> std::vector<Entry> {
  std::vector<Entry> phone_book = {// using initializer_list
                                   {"POO", 123456},
                                   {"PEE", 234567},
                                   {"PAA", 345678}};
  for (Entry e : phone_book) {
    en.push_back(e);
  }
  en.reserve(en.size() * 2); // double the size

  // make a mess.
  try {
    phone_book[phone_book.size()] = {"Joe", 9999};
  } catch (std::out_of_range &) {
    std::cerr << "range error\n";
  }
  return en;
}

// push_back will push a copy.
// when needs pointer just push back pointer.
auto dov(std::vector<Entry> &&phone_book) -> std::vector<Entry> {
  // vector can be thought as a kind of smart pointer.

  for (Entry e : phone_book) {
    std::cout << e.name << e.number << std::endl;
  }
  std::cout << phone_book.capacity() << std::endl;
  std::cout << phone_book.size() << std::endl;

  phone_book = movev(std::move(phone_book));
  return phone_book;
}

// list is by default double linked list
// single linked list is called forward list.
auto lf(std::vector<Entry> &&phone_book, const Entry &ee,
        std::vector<Entry>::iterator p, std::vector<Entry>::iterator q) {
  phone_book.insert(p, ee);
  phone_book.erase(q);
  return [&]() -> std::forward_list<Entry> {
    std::forward_list<Entry> fl;
    for (const auto e : phone_book) {
      std::copy(phone_book.begin(), phone_book.end(), fl.begin());
    }
    return fl;
  };
}

bool Entry::operator<(const Entry &y) { return name < y.name; }

// algorithms.
// based on iterator. use begin and end to specify a range.
auto vsort(std::vector<Entry> &vec, std::list<Entry> &lst) -> std::list<Entry> {
  std::list<Entry> res;
  std::sort(vec.begin(), vec.end());
  std::unique_copy(vec.begin(), vec.end(),
                   lst.begin()); // lst.size() >= unique_copy of vec.
  std::unique_copy(vec.begin(), vec.end(),
                   std::back_inserter(res)); // back_inserter will init res.
  return res;
}

static inline auto has_c(const std::string &s, char c) -> bool {
  return std::find(s.begin(), s.end(), c) != s.end();
}

template <typename T> using Iterator = typename T::iterator;

template <typename C, typename V>
auto find_all(C &c, V v) -> std::vector<typename C::iterator> {
  std::vector<Iterator<C>> res;
  for (auto p = c.begin(); p != c.end(); ++p) {
    if (*p == v)
      res.push_back(p);
  }
  return res;
}

// stream iterator.
// ios_base <-- ios <-- istream <-- ifstream
namespace StreamIterator {
static std::ostream_iterator<std::string>
    oo(std::cout); // write string to cout.
static std::istream_iterator<std::string> ii{std::cin};

// ifstream is a istream that can be attached to a file.
auto someio() -> void {
  std::string filename = "somename";
  double d = 3.14;
  // play with exception
  try {
    std::ofstream(filename, std::ios::binary)
            .write(reinterpret_cast<char *>(&d), sizeof(d))
        << 123 << "abc";
  } catch (std::exception &err) {
    std::cerr << err.what() << std::endl;
  }

  // just throw
  std::ifstream istrm(filename, std::ios::binary);
  if (!istrm.is_open()) {
    throw std::runtime_error("file not found");
  }
  istrm.read(reinterpret_cast<char *>(&d), sizeof(d));
  int n;
  std::string s;
  if (istrm >> n >> s)
    std::cout << "read back from file" << d << ' ' << n << ' ' << std::endl;
}

auto ffind() {
  const std::map<std::string, int> &m{
      {"good", 12},
      {"bad", 2},
      {"happy", 59},
      {"sad", 90},
  };
  auto greater_than = [](const std::pair<std::string, int> &r) {
    return r.second > 42;
  };
  return std::find_if(m.begin(), m.end(), greater_than);
}

auto paraf() {
  std::vector<std::string> v{"a", "b", "dd", "aa", "aaa"};
  std::sort(std::execution::par, v.begin(), v.end());
}

} // namespace StreamIterator

namespace Utilities {
std::mutex m;
std::vector<int> shared{1, 2, 3, 4, 5};

auto threadf() -> void {
  {
    // acquire here.
    std::scoped_lock<std::mutex> lck{m};
    std::for_each(shared.begin(), shared.end(), [](int a) { return a + 1; });
    // release here.
  }
}

auto uniqp(std::unique_ptr<std::string> str) -> std::unique_ptr<std::string> {
  str->append("good");
  {
    std::unique_ptr<std::string> str1 =
        std::make_unique<std::string>(str); // moved!
    if (str == nullptr) {
      std::cout << "null" << std::endl; // now str is null.
    }
    // str1 get released.
  }
  return str; // this is an error since str is moved.
}

// use move to safe space when swapping.
template <typename T> auto mswap(T &a, T &b) {
  T tmp{std::move(a)};
  a = std::move(b);
  b = std::move(tmp);
}

template <typename T, typename... Args>
auto mkunique(Args &&...args) -> std::unique_ptr<T> {
  return std::unique_ptr<T>(new T(std::forward<Args>(args)...));
}

auto arrayf() {
  std::array<int, 4> arr = {1, 2, 3, 4};
  // c interface
  auto to_vec = [&arr](int *p, int sz) -> std::vector<int> {
    std::vector<int> v;
    std::copy(arr.begin(), arr.end(), std::back_inserter(v));
    return v;
  };
  return to_vec(&arr[0], arr.size());
}

auto bsetf() -> std::array<std::bitset<10>, 5> {
  std::bitset<10> bs{"11100011"};
  std::bitset<10> bs1 = ~bs;
  std::bitset<10> bs2 = bs & bs1;
  std::bitset<10> bs3 = bs << 2;
  std::bitset<10> bs4 = bs | bs2;

  return std::array{bs, bs1, bs2, bs3, bs4};
}

auto ptuple() -> std::string {
  std::tuple<std::string, int, double> t1{"Shark", 12, 2.14};
  auto t2 = std::make_tuple(std::string{"good"}, 3, 1.2);
  std::tuple t3{"Cod", 20, 9.9};
  std::string s = std::get<0>(t1);
  return s;
}

} // namespace Utilities

namespace Concurrency {}
