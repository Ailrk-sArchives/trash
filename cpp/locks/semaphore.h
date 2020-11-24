#ifndef SEMAPHORE_H
#define SEMAPHORE_H

#include <condition_variable>
#include <mutex>

/* Cons of semaphore:
 *  1. global variable in nature.
 *  2. no linguistic connection with data.
 *  3. access can come from anywhere
 *  4. overloaded purposes: mutual exclusion and scheduling constraints.
 *  5. hard to use
 */

class Semaphore {
  const size_t num_premissions;
  size_t avail;
  std::mutex m;
  std::condition_variable cv;

public:
  // default costructor.
  explicit Semaphore(const size_t &num_premissions = 1)
      : num_premissions(num_premissions), avail(num_premissions) {}

  // copy construtor. Should not copy state mutex or cv
  Semaphore(const Semaphore &s)
      : num_premissions(s.num_premissions), avail(s.avail) {}

  void wait() {
    std::unique_lock<std::mutex> lk{m};
    cv.wait(lk, [this] { return avail > 0; });
    avail--;
    lk.unlock();
  }

  void signal() {
    {
      std::lock_guard<std::mutex> lg{m};
      avail++;
    }
    cv.notify_one();
  }

  size_t available() const { return avail; }
};

#endif /* ifndef SEMAPHORE_H */
