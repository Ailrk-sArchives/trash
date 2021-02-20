#include <iostream>
#include <thread>

template <typename T, T t> class Atomic_ {

  void operator++() {
    __asm__ __volatile__("lock;"
                         "incq %0;"
                         : "+m"(t)
                         :
                         :);
  }

  void operator--() {
    __asm__ __volatile__("lock;"
                         "decq %0;"
                         : "+m"(t)
                         :
                         :);
  }

  void cmp_n_xhg() {}
};
