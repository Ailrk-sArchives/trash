#include "semaphore.h"
#include <functional>
#include <mutex>

template <typename T> class BoundBuffer {
  T *const buffer;
  Semaphore m;
  Semaphore empty; // count of free slots
  Semaphore full;  // count of used slots.

public:
  // empty has the bound initially.
  BoundBuffer(int bound) : m(1), empty(bound), full(0) {}
  void producer();
  void consumer();
};

template <typename T> void BoundBuffer<T>::producer() {
  // produce item
  empty.wait(); // one fewer slot or wait
  m.wait();
  // add to buf
  m.signal();
  full.signal();
}

template <typename T> void BoundBuffer<T>::consumer() {
  full.wait(); // wait til there's an item (initally full = 0, so it will lock)
  m.wait();
  // remove item from buf
  m.signal();
  empty.signal();
}
