#include <vector>

// http://eel.is/c++draft/class.dtor

// NOTE1: once dtor is called the lifetime of the objec tends.
// NOTE2: it's ub to invoke dtor on a object that it's lifetime is already
//        finished.

struct X {
  X(int);
  ~X();
};

void f(X *p);


// NOTE3: once use of explicit dtor is to destruct the object but still holds
//        the memory.
// use placemnet new to allocate the memory first, then emplace the object
// in that memory.
void g() {
  char *buf = new char[sizeof(X)];
  X *p = new (buf) X(222);
  f(p);
  // cleanup the object but still holds the memory.
  p->X::~X();

  p = new (buf) X(111);
  f(p);
  p->X::~X();
}

int main(void) {
  std::vector<int> v{1, 2, 3};
  v.~vector<int>();

  {}
  return 0;
}
