#include <iostream>
#include <vector>

struct S {
  S() { puts("S()"); }
  S(int) { puts("S(int)"); }
  S(const S &) noexcept { puts("S(const S&)"); }
  S(S &&) noexcept { puts("S(S &&)"); }
  S &operator=(const S &) {
    puts("operator=(const S&)");
    return *this;
  }

  S &operator=(S &&) {
    puts("S &operator=S &&()");
    return *this;
  }

  ~S() { puts("~S()"); }
};

// return a default temporary.
S get_value() { return {}; }

int main(void) {
  {
    std::vector<S> vec;
    vec.push_back(S{1});
    // S(int)
    // S(&&S)
    // ~S()   // moved from object still need to be destructed.
    // ~S()   // this is when the object is destroyed.
  }
  puts("");

  {
    std::vector<S> vec;
    vec.emplace_back(S{1});
    // it's the same.
  }

  puts("");
  {
    std::vector<S> vec;
    vec.emplace_back(1);
    // This time we don't have move anymore.
  }

  puts("");
  {
    // assign rvalue to a const lvalue reference
    // You cannot take a non const reference of temporary
    const auto &val = get_value();
    puts("hello");
  }

  return 0;
}
