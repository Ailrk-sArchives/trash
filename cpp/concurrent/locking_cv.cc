#include <condition_variable>
#include <iostream>
#include <memory>
#include <mutex>
#include <thread>
#include <vector>

// Use recursive mutex, or reentrance lock, you can
// lock a same lock multiple times
struct Complex {
  std::recursive_mutex mut;
  int i;

  void mul(int x) {
    std::lock_guard<std::recursive_mutex> guard(mut);
    i *= x;
  }

  void div(int x) {
    std::lock_guard<std::recursive_mutex> guard(mut);
    i /= x;
  }

  // This function only works for reentrance lock.
  // If mut is just a ordinary mutex,
  // Once you enter both, mut will be lock.
  // Then you cannot preceed into mul because it's
  // lock_guard requires mut to be unlocked.
  void both(int x, int y) {
    std::lock_guard<std::recursive_mutex> guard(mut);
    mul(x);
    div(y);
  }
};

// Sometimes you don't want to lock a lock forever.
// Instead you want to say, I want to lock this mutex for
// a timeout, like 2000 ms, and then free it.
// Because I know locking it for 2000ms is long enough
// for it's purpose.
// This approach works only when you know the timeout
// is long enough to keep all threads properly synchronized.
// If you cann't guarantee that, the lock will be completely
// useless.
struct TimedLocking {
  std::vector<std::thread> threads;
  std::timed_mutex tmut;

  void work() {
    std::chrono::milliseconds timeout(500);
    for (int i = 0; i < 2; ++i) {
      if (tmut.try_lock_for(timeout)) {
        std::cout << std::this_thread::get_id() << ": do work with the mutex"
                  << std::endl;
        std::chrono::milliseconds sleep_duration(250);
        std::this_thread::sleep_for(sleep_duration);
        tmut.unlock();
      } else {
        std::cout << std::this_thread::get_id() << ": do work without mutex"
                  << std::endl;
        std::chrono::milliseconds sleep_duration(100);
        std::this_thread::sleep_for(sleep_duration);
      }
    }
  }

  // This is a good example of  constructor with side effect.
  // Whenever you create this thing it will execute
  // forever. Very stincky.
  TimedLocking() {
    for (int i = 0; i < 2; ++i) {
      // needs to capture this by value.
      // this is a raw pointer so be careful.
      threads.push_back(std::thread([this]() { this->work(); }));
    }

    for (auto &t : threads) {
      t.join();
    }
    std::cout << "end" << std::endl;
  }
};

// call_once can be used to make sure a function
// is called once no matter how many threads there are.
// once_flag is really just an atomic bool
struct CallOnce {
  std::once_flag flag;
  std::vector<std::thread> threads;

  void do_something() {
    std::call_once(flag, []() { std::cout << "call once!" << std::endl; });
    std::cout << "call each time" << std::endl;
  }

  CallOnce() {
    for (int i = 0; i < 4; ++i) {
      threads.push_back(std::thread([this]() { this->do_something(); }));
    }

    for (auto &t : threads) {
      t.join();
    }
    std::cout << "end" << std::endl;
  }
};

template <typename T> struct BoundedBuffer {
  // here we uses a unique_ptr to manage the ownership
  std::unique_ptr<std::vector<T>> buffer;
  int capacity;
  int front;
  int rear;
  int count;
  std::mutex lock;

  std::condition_variable not_full;
  std::condition_variable not_empty;

  BoundedBuffer(int capacity)
      : capacity(capacity), front(0), rear(0), count(0),
        buffer(std::make_unique<std::vector<T>>()) {}

  ~BoundedBuffer() {}

  void add(T data) {
    // unique lock is also a wrapper to manage a lock.
    // It's necessary to use unique_lock with condition
    // variables.
    std::unique_lock<std::mutex> l(lock);

    // wait takes a lock and a preducate function.
    // it blocks until the predicate is true.
    // otherwise the thread will sleep.
    not_full.wait(l, [this]() { return count != capacity; });

    buffer->at(front) = data;
    rear = (rear + 1) % capacity;
    ++count;
    l.unlock();

    // To wake up a thread that is waiting for a condition
    // variable, you call nofity.
    //
    not_empty.notify_one();
  }

  int get() {
    std::unique_lock<std::mutex> l(lock);

    not_empty.wait(l, [this]() { return count != 0; });

    int result = buffer->at(front);
    front = (front + 1) % capacity;
    --count;

    l.unlock();
    not_full.notify_one();
    return result;
  }
};

class ProducerComsumerProblem {

public:
  ProducerComsumerProblem() {
    BoundedBuffer<int> buffer{200};

    std::thread p1([&buffer]() { producer(1, buffer); });
    std::thread p2([&buffer]() { producer(2, buffer); });
    std::thread c1([&buffer]() { consume(1, buffer); });
    std::thread c2([&buffer]() { consume(2, buffer); });
    std::thread c3([&buffer]() { consume(3, buffer); });

    c1.join();
    c2.join();
    c3.join();
    p1.join();
    p2.join();
    std::cout << "end" << std::endl;
  }

  static void consume(int id, BoundedBuffer<int> &buffer) {
    for (int i = 0; i < 50; ++i) {
      int value = buffer.get();
      std::cout << "Consumer " << id << " got " << value << std::endl;
      std::this_thread::sleep_for(std::chrono::milliseconds(200));
    }
  }

  static void producer(int id, BoundedBuffer<int> &buffer) {
    for (int i = 0; i < 75; ++i) {
      buffer.add(i);
      std::cout << "Producer: " << id << " produced " << i << std::endl;
      std::this_thread::sleep_for(std::chrono::milliseconds(50));
    }
  }
};

int main(void) {
  Complex comp;
  comp.both(2, 3);
  std::cout << comp.i << std::endl;

  std::cout << "=====" << std::endl;

  TimedLocking timed;

  std::cout << "=====" << std::endl;

  CallOnce once;

  std::cout << "=====" << std::endl;

  ProducerComsumerProblem pcp;
  std::cout << "=====" << std::endl;

  return 0;
}
