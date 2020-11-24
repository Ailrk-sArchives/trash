#include <iostream>

template <typename T, std::size_t N>
constexpr std::size_t array_size(T (&)[N]) noexcept {
  return N;
}

template <typename T> void universal(T &&t);
template <typename T> void byval(T t);

template <typename T>
void initlist(std::initializer_list<T> init);

int main(void) {
  // array with known size.
  char arr[3] = {'a', 'b', 'c'};
  auto s = array_size(arr);
  if (s == 3) {
    std::cout << "yeet" << std::endl;
  }

  { // passing lvalue to universal reference .
    const int a = 10;
    // here T is deduced to const int&, parameter type is also const int &
    universal(&a);

    // here T is deduced to int, parameter type is int&&
    universal(10);
  }

  { // pasing by value drop the constness and volatile.

    volatile const int a = 10;
    const int &b = 10;
    // T has type int, a has type int.
    // because a is copied it doesn't matter if it passed in as const or not.
    byval(a);

    // reference is dropped here.
    byval(b);
  }

  { // auto type deduction

    // auto is just int
    auto x = 28;

    // you can add modifier to auto.
    // auto is still int.
    const auto cs = x;

    // still int
    const auto &rx = x;

    // x is lvalue reference of int => auto is int&
    auto &&uref1 = x;

    // cs is const lval ref, so constness preserve. auto is cosnt int&.
    auto &&unref2 = cs;

    // assigning a rvalue to rval reference. get int&&
    auto &&uref3 = 27;
  }

  // Only catch for auto
  // You can't decude the type of initializer list.
  // and you need to manually annotate that.
  { // the different part of auto type deduction compare with template.

    auto x1 = 27;

    auto x2(x1);

    // this get initialized to 27
    int x33 = {27};

    // this is just an initializer list.
    auto x3 = {27};

    auto x4{27};

    // why you can't deduce this?
    // auto x5 = {1, 2, 3.0};

    // but you can deduce here.
    initlist({1,2,3});

  }

  return 0;
}
