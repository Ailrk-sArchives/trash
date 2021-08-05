#include <iostream>

// increment rr
void incr(int &rr) { rr++; }

void incr_rval(int &&rr) { rr++; }

int main(void) {
  double d = 1.0;
  int i = 1;

  // this doesn't work, because we are passing double to int&, which will
  // creates a temporary by performing the casting (int)d.
  // Then the increment will act on the temporary instead of the original double
  // value.
  //
  // So we forbid passging rvalue to non const reference one thing for all.
  // incr(d);

  incr(i);

  std::cout << "Original double d: " << d << std::endl;
  std::cout << "Original int i: " << i << std::endl;

  std::cout << "inc int to void incr(int &rr): " << i << std::endl;

  // here we cast i to int&&, but it really still works like lvalue reference
  incr_rval(std::move(i));
  std::cout << "inc int to void incr(int &&rr): " << i << std::endl;

  // this cast d to a rvalue int, and the original double is not modified.
  // Of course the processing of casting will create a temporary, the temporary
  // is copied.
  incr_rval(d);   // => 1.0
  std::cout << "inc double to void incr(int &&rr): " << d << std::endl;

  return 0;
}
