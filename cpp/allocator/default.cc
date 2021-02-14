#include <iostream>

// default allocator from C++ 03
template <typename T> struct Allocator {

  using size_type = size_t;
  using difference_type = ptrdiff_t;
  using pointer = T *;
  using const_pointer = T const *;
  using reference = T &;
  using const_reference = T const &;
  using value_type = T;

  template <typename U> struct rebind { using other = Allocator<U>; };

  // 4 primitive operations of an allocator.

  //
  inline pointer allocate(size_type n, Allocator<T>::const_pointer hint = 0) {
    return static_cast<T *>(::operator new(n * sizeof(T)));
  }

  //
  inline void deallocate(pointer p, size_type n) { ::operator delete(p); }

  // note in modern c++ you don't need to implement
  // construct and destory, because there are corresponding
  // default in allocator_traits.
  inline void construct(pointer p, T const &val) { ::new ((void *)p) T(val); }

  //
  inline void destroy(pointer p) { return ((T *)p)->~T(); }
};

template <typename T, typename U>
inline bool operator==(Allocator<T> const &, Allocator<U> const &) {
  return true;
}

template <typename T, typename U>
inline bool operator!=(Allocator<T> const &, Allocator<U> const &) {
  return false;
}
