#include <iostream>
#include <memory>

struct pod_t {
  int a;
  int b;

  pod_t(int a, int b) : a(a), b(b) {}
};

int main(void) {
  std::unique_ptr<pod_t> ptr = std::make_unique<pod_t>(1, 2);

  auto pod1 = *ptr;

  int *cptr = new int(1);

  std::cout << "cptr, check where is the heap: " << cptr << std::endl;
  std::cout << "unique ptr is on the stack: " << &ptr << std::endl;
  std::cout << "unique ptr holds a heap pointer: " << ptr.get() << std::endl;
  std::cout << "ptr: " << &(*ptr) << " It's clearly on heap" << std::endl;
  std::cout << "ptr1: " << &pod1 << " It's on stack" << std::endl;

  return 0;
}
