#ifndef QUEUE_H
#define QUEUE_H

#include "semaphore.h"
#include <cstring>
#include <thread>

template <typename T> class Queue {
  const size_t size;
  T *const arr;
  int start = 0, end = 0;
  Semaphore write, read;
  std::recursive_mutex writemtx, readmtx;

public:
  explicit Queue(const size_t size = 1)
      : size(size), arr(new T[size]), write(size), read(0) {}

  explicit Queue(const Queue &q)
      : size(q.size), arr(new T[size]), start(q.start), end(q.end),
        write(q.write), read(q.read) {
    std::lock_guard<std::recursive_mutex> lg(q.writemtx);
    std::memcpy(arr, q.arr, size * sizeof(T));
  }

  virtual ~Queue() { delete[] arr; }

  void add(const T &obj) {
    std::lock_guard<std::recursive_mutex> lg(writemtx);
    write.wait();
    arr[end++] = obj;
    end %= size;
    read.signal();
  }

  bool offer(const T &obj) {
    std::lock_guard<std::recursive_mutex> lg(writemtx);
    if (!full()) {
      add(obj);
      return true;
    }
    return false;
  }

  const T &front() {
    std::lock_guard<std::recursive_mutex> lg(readmtx);

    read.wait();
    read.signal();
    return arr[start % size];
  }

  bool empty() const { return !read.available(); }

  bool full() const { return !write.available(); }
};

#endif /* ifndef QUEUE_H */
