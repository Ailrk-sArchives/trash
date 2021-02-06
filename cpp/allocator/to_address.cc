#include <memory>

// Note this is a C++ 20 feature.

template <typename A> auto allocator_new(A &a) {
  // allocate() allocate memory
  auto p = a.allocate(1);
  try {
    // construct create an object in the memory that just allocated.
    // if construct failed just call ::new()
    // it takes a raw pointer.
    std::allocator_traits<A>::construct(a, std::to_address(p));
  } catch (...) {
    a.deallocate(p, 1);
    throw;
  }
  return p;
}

template <typename A>
void allocator_delete(A &a, typename std::allocator_traits<A>::pointer p) {
  std::allocator_traits<A>::destroy(a, std::to_address(p));
  a.deallocate(p, 1);
}

int main(void) {

  std::allocator<int> a;
  auto p = allocator_new(a);
  allocator_delete(a, p);

  return 0;
}
