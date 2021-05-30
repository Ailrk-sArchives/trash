// https://onedrive.live.com/view.aspx?resid=F1B8FF18A2AEC5C5!1062
// Scott Meyeres Universal References in c++11.
#include <iostream>
#include <vector>
// T&: lvalue referece
// T&&: rvalue reference or universal reference
// universial reference happen when there is type deduction.
// universial reference can be deduced into both l and r value ref.

// four conditions for universal reference to emerge
// - function template parameter template <typename _InputIter>
// - auto declaration
// - typedef declaration
// - decltype expression.

struct Widget {};

// some examples.
namespace N1 {
void f(Widget &&w);       // rvalue reference.
Widget &&var1 = Widget(); // rvalue reference.
auto &&var2 = var1;       // lvalue reference.

template <typename T> void f(std::vector<int> &&p); // rvalue reference.

template <typename T> void f(T &&p); // uvalue reference.

}; // namespace N1

// ex 1
namespace E1 {
template <typename T> void f(T &&p); // ureference.

void foo() {
  Widget w;
  f(w);            // lvalue reference.
  f(std::move(w)); // rvalue reference.
  f(Widget());     // rvalue reference.
}
} // namespace E1

// ex 2
namespace E2 {
std::vector<int> v;
auto &&val = 10;       // rvalue reference.
auto &&element = v[5]; // lvalue reference;
} // namespace E2

// fyi range based for loop
namespace FYI {
template <typename T> void range(T &&__param) {
  auto &&__range = __param; // use uref to accept all types.
  // ...
}
} // namespace FYI

// Ex 3
namespace E3 {
template <typename T> struct Gardget {
  Gardget(Gardget &&rhs); // rref (undeduced).
};

template <typename T> struct Gardget1 {
  template <typename U> Gardget1(U &&rhs); // uref (deduded by U.)
};

} // namespace E3

// overloading + uref doesn't make sense
// counter example
namespace CEX {
class CounterEx {
public:
  template <typename T> void do_work(const T &param); // only for const lval
  template <typename T> void do_work(T &&param);      // match everthing
};
} // namespace CEX

// rref -> uref (std::move -> std::forward)
namespace MoveForward {
void doWork(const Widget &param) { // copy
  Widget param2 = param;
}

void doWork(Widget &&param) {
  Widget param2 = std::move(param); // move
}

template <typename T> void dowork(T &&param) {
  Widget param2 = std::forward<T>(param);   // forword -> copy and move.
}

} // namespace MoveForward


// c++ reference collapsing.
// T& &   => T&
// T&& &  => T&
// T& &&  => T&
// T&& && => T&&
// in other word,
// rref to rref => rref
// lref to anything => lref
//
// writing T&& is really leting the compiler doing the deduction
// based on the type.
// if T&& is deduced as a left reference, it will collapse into
// a lref
// onthe other hand, a rref T&& will be decuded to T itself.
