#include <deque>
#include <iostream>
#include <list>
#include <memory>
#include <stdlib.h>
#include <string>

/////////////////////////////
// varadic template
template <typename... Args> class VClass {};

// specialization
VClass<> v1;
VClass<int> v2;
VClass<double> v3;

/////////////////////////////
// this is not supported until 17
// type deduction on non type template parmeters.
template <auto x> constexpr auto constant = x;

// with this at hand you can treat templates as
// compile time fuction.
// from type to term level.
auto x1 = constant<1>;
auto x2 = constant<2>;
auto x3 = constant<'a'>;

/////////////////////////////
// templates as template paramters.
// higher kinded type
template <typename T, template <typename U, int I> typename Arr> class Klcass1 {
  T t;
  Arr<T, 10> a;
};

/////////////////////////////
// default template arguments.
template <typename T, typename Allocator = std::allocator<T>> class Vector {};

// use default allocator.
Vector<int> vec1;

// we can also swap out our own allocator.
// This is an arena allocator
template <typename T> class ArenaAlloc {
private:
  // blocks are linked list on heap.
  template <size_t BlockSize> struct Arean {
    struct Arena *next;
    struct Arena *pre;
    char block[BlockSize];
  };

public:
  T *allocate(T &&) {
    T *ptr = static_cast<T *>(malloc(sizeof(T)));
    return ptr;
  }

  void deallocate() { return; }
};

// now yor vector will be arena allocated. (fake)
Vector<int, ArenaAlloc<int>> vec2;

/////////////////////////////
// template specialization
template <typename K, typename V> class MyMap {};

// partial specialization.
template <typename K, typename V> class MyMap<K*, V*> {};

// you might want to handle the case ofstring as key
// differently.
template <typename V> class MyMap<std::string, V> {};

// or you might want to pop as log as you get get queue?
// who knows.
template <typename K> class MyMap<K, std::deque<int>> {};

/////////////////////////////
// specialization of function templates.

// first declare the most generic version
template <typename... Args> void f(Args... args) {
  ((std::cout << args), ...) << std::endl;
}

// now take at least one
template <typename T, typename... Args> void f(T t, Args... args) {
  std::cout << "T and args" << std::endl;
  ((std::cout << args), ...) << std::endl;
}

// only one argument.
// This case might conflict above?
template <typename T> void f(T t) { std::cout << "T!" << std::endl; }

template <> void f(int x) { std::cout << "int: " << x << std::endl; }

int main(void) {
  f(1, 2, 3);
  std::cout << "==" << std::endl;
  f(1);
  std::cout << "==" << std::endl;
  f(12.2);
  std::cout << "==" << std::endl;

  return 0;
}
