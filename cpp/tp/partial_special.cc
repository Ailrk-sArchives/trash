#include <iostream>
#include <vector>

// class templates an be paritally specialized, and resulting class
// is still a template.
//
// Partial specialization is userful when a template has multiple
// types and only some of them need to be specialized.
// Or template has only one type but need to be specialized for
// different varaitions ""
//

template <typename T> struct PTS {
  enum { IsPointer = 0, IsPoitnerToMember = 0 };
};

template <typename T> struct PTS<T *> {
  enum { IsPointer = 1, IsPoitnerToMember = 0 };
};

template <typename T, typename U> struct PTS<T U::*> {
  enum { IsPointer = 0, IsPoitnerToMember = 1 };
};

struct S {};

int main(void) {

  std::cout << "PTS<S>: " << PTS<S>::IsPointer << " "
            << PTS<S>::IsPoitnerToMember << std::endl;

  std::cout << "PTS<S*>: " << PTS<S *>::IsPointer << " "
            << PTS<S *>::IsPoitnerToMember << std::endl;

  std::cout << "PTS<int S::*>" << PTS<int S::*>::IsPointer << " "<<
    PTS<int S::*>::IsPoitnerToMember<< std::endl;

  return 0;
}
