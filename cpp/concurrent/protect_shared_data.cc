#include <mutex>
#include <thread>

// the prefereable way to lock a scope.
// It uses RAII so you don't need to care about
// to unlock.
// But the problem of this approach is you cannot
// lock in once place and unlock
// from the other, so you can't use mutex as signal.
struct Counter1 {
  int value;
  std::mutex mut;

  Counter1() : value(0) {}
  void inc() {
    std::lock_guard<std::mutex> guard(mut);
    ++value;
  }
  void dec() {
    std::lock_guard<std::mutex> guard(mut);
    --value;
  }
};

// just use mutex to lock the function call.
// to be exception safe, the increment statement
// is also wrapped in a try catch block.
// So even if an exception happend, the lock can
// still be freed.
struct Counter2 {
  int value;
  std::mutex mut;

  Counter2() : value(0) {}
  void inc() {
    mut.lock();
    try {
      ++value;

    } catch (std::exception e) {
      mut.unlock();
      throw e;
    }
    mut.unlock();
  }
  void dec() {
    mut.lock();
    try {
      ++value;

    } catch (std::exception e) {
      mut.unlock();
      throw e;
    }
    mut.unlock();
  }
};
