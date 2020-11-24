#include <iostream>

#include <list>

// type aliasing can be used with tempaltes directly.
template <typename T> class MyAlloc {};
template <typename T> using MyAllocList = std::list<T, MyAlloc<T>>;

// it's hard to do with typedef, you need to wrap it within a struct.
template <typename T> struct MyAllocList1 {
  typedef std::list<T, MyAlloc<T>> type;
};

constexpr int foo() { return 1; }

int main(void) {
  {
    std::remove_const<const int>::type b;

    std::remove_const_t<const int> a;
    a = 10;
    auto a1 = a;
  }

  {
    std::remove_reference_t<int &&> b;
    b = 10;
    auto b1 = b;
  }

  {
    int a = 10;
    std::add_lvalue_reference_t<int> c(a);
    auto c1 = c;

    auto &&c2 = c;
  }

  return 0;
}
