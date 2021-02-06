// https://stackoverflow.com/questions/31978324/what-exactly-is-stdatomic
#include <atomic>
#include <iostream>
#include <thread>

// A class works around an atomic variable.
struct Flag {
  // make an atomic
  std::atomic<int> foo;
  mutable std::atomic_int ff;

  Flag() : foo(0) {}

  void set(int x) { foo.store(x, std::memory_order_relaxed); }

  int set_to(int x) {
    // atomically do: old = foo; foo = x;
    int old = foo.exchange(x);
    return old;
  }

  void print() {
    int x;
    do {
      x = foo.load(std::memory_order_relaxed);
    } while (x == 0);
    std::cout << "reuslt: " << x << std::endl;
  }
};

// This is an terrible example because it
// really don't need atomic.
struct Flag1 {
  // make an atomic
  int foo;

  Flag1() : foo(0) {}

  void set(int x) { foo = x; }

  void print() {
    int x;
    do {
      x = foo;
    } while (x == 0);
    std::cout << "reuslt: " << x << std::endl;
  }
};

int main(void) {
  Flag1 flag;

  std::thread t1([&flag]() { flag.print(); });

  std::thread t2([&flag]() { flag.set(10); });

  t1.join();
  t2.join();

  return 0;
}
