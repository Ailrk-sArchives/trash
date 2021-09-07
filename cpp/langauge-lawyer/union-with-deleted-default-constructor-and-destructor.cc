#include <iostream>
#include <cstring>

///////////////////////////////////////////////////////////////////////////////
// Case:
//    A union with non trivial default constructor and destructor will have it's default constructor and destructor deleted.

// https://eel.is/c++draft/class.union#general-4

// NOTE1: when implementing tagged union, at the moment you have types with non
// trivial constructor or destructor, you need to provide constructor and
// destructor for the union.

struct NoDefaultCtor {
  NoDefaultCtor(const NoDefaultCtor&) {
    std::cout << "User defined copy constructor" << std::endl;
  }
};


union U1 {
  int a;
  int b;
};

union U2 {
  int a;
  NoDefaultCtor n;

  // NOTE: we need to supply a constructor for it.
  U2() {
    std::memset(this, 0, sizeof(U2));
  }
};


int main(void)
{
  U1 u1;
  U2 u2;

  return 0;
}
