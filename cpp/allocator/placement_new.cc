#include <iostream>

struct Vector3 {
  int x;
  int y;
  int z;
};

template <typename T> T *newnew(unsigned char *buf) {
  std::cout << "allocating size " << sizeof(T) << ".." << std::endl;

  T *ptr = new (buf) T;

  return ptr;
}

int main(void) {
  alignas(char) unsigned char buf1[sizeof(char)];
  alignas(Vector3) unsigned char buf2[sizeof(Vector3)];

  auto x = newnew<char>(buf1);
  auto y = newnew<Vector3>(buf2);
  *x = 'c';
  *y = Vector3{1, 2, 3};

  std::cout << *x << std::endl;
  std::cout << y->x << std::endl;
  std::cout << y->y << std::endl;
  std::cout << y->z << std::endl;

  // call the destructor mannually.
  y->~Vector3();
  return 0;
}
