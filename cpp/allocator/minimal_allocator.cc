#include <iostream>
#include <memory>
#include <vector>

// after you implemented this parts you get a workable
// customized allocator.
template <typename T> struct MinimalAllocator {

  using value_type = T;

  template <typename U>
  MinimalAllocator(MinimalAllocator<U> const &) noexcept {}
  MinimalAllocator() = default;
  ~MinimalAllocator() = default;

  T *allocate(size_t n);
  void deallocate(T *p, size_t);
};

template <typename T> T *MinimalAllocator<T>::allocate(size_t n) {
  std::cout << "allocating" << std::endl;
  return static_cast<T *>(::operator new(n * sizeof(T *)));
}

template <typename T> void MinimalAllocator<T>::deallocate(T *p, size_t n) {
  std::cout << "deallocating" << std::endl;
  ::operator delete(p);
}

template <typename T>
bool operator==(MinimalAllocator<T> const &, MinimalAllocator<T> const &) {
  return true;
};

template <typename T>
bool operator!=(MinimalAllocator<T> const &, MinimalAllocator<T> const &) {

  return false;
};
