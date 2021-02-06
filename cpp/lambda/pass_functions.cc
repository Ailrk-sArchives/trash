#include <functional>
#include <iostream>

int function(int a) { return a + 1; }

class FunctionObject {
  int x_;

public:
  FunctionObject(const int x) : x_(x) {}

  int operator()(int a) { return a + x_; }
};

template <typename Fn> int template_caller(Fn f, int arg) { return f(arg); }

int function_caller(std::function<int(int)> f, int arg) { return f(arg); }

int main(void) {
  int x = 3;
  volatile int y;

  FunctionObject fo(x);

  auto lambda = [=](int a) { return a + x; };
  y = template_caller(function, 5);
  y = template_caller(fo, 5);
  y = template_caller(lambda, 5);

  y = function_caller(function, 5);
  y = function_caller(fo, 5);
  y = function_caller(lambda, 5);
  return 0;
}
