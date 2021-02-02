#include <iostream>
#include <memory>

int main(void) {

  // default allocator used for all stl containers.
  // default allocator is stateless, instance given to
  // the allocator are interchangable.
  std::allocator<int> a1;

  int *a = a1.allocate(1); // allocate 1 on heap
  a1.deallocate(a, 1);     // free

  // default allocator for string
  std::allocator<std::string> a2;
  // still default allocator for string
  // rebind int to string.
  decltype(a1)::rebind<std::string>::other a2_1;

  // still the same
  std::allocator_traits<decltype(a1)>::rebind_alloc<std::string> a2_2;



  return 0;
}
