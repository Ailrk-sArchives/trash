// https://stackoverflow.com/questions/48550134/software-transaction-memorystm-vs-compare-and-swapcas
// https://www.youtube.com/watch?v=ZQFzMfHIxng

// Atomic is slightly faster then mutex.
// but spinlock and mutex run roughly at the same speed.

// Atomic operations have to wait for cache line access.

#include <atomic>

// CAS
class CompareAndSwap {
  std::atomic<int> x;

  CompareAndSwap() : x(0) {}

  void inc() {
    // read the atomic value first.
    int x0 = x;

    // if x is not change, thus it's still equals to x1,
    // then swap it to x0+1.
    // If someone did change x, x != x0, so
    // compare exchange failed.
    // Put it in a while loop so we can try it again.
    while (!x.compare_exchange_strong(x0, x0 + 1))
      ;
  }
};

int main(void) { return 0; }
