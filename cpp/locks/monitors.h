#ifndef MONITOR_H
#define MONITOR_H

#include "semaphore.h"
#include <condition_variable>

/*
 * Two types of monitors:
 *  1. Mesa monitor
 *    - thread the signals keeps the lock
 *    - Waiting thread waits for the lock
 *  2. Hoare monitor
 *    - The thread signals gives up the lock and waiting thread
 *      get the lock
 *    - When the waiting thread is now executing exits or wait again,
 *      it release the lock back to the signaling thread.
 *
 * Monitors:
 *  1. Guarantee mutual exclusion.
 *  2. Only one thread can execute a monintor at a time.
 *
 * Monitors has:
 *  1. a lock
 *  2. 0 or more conditional variables.
 *  3. provides mutual exclusion for shared data.
 *  4. the lock is used to
 *    1. make sure only one thread is active.
 *    2. mutual exclusion for shared data.
 *  5. conditional variables enable threads to go sleep in critical sec.
 *     by releasing their lock at the same time it puts the threads to sleep.
 *
 * Use monitors to:
 *  1. Encapsulates shared data you want to protect.
 *  2. Acquires mutex at the start.
 *  3. Operates on the shared data.
 *  4. Temporarily releases the mutex if it can't complete.
 *  5. Requires the mutext when it can continue.
 *  6. Release the mutex at the end.
 *
 * Conditional variable:
 *  Queue of threads waiting for something inside a critical sectinon.
 *
 *  When we are waiting, we want to sleep in the critical section until
 *  get signaled.
 *  But you can't hold the lock while waiting otherwise you block otheres.
 *  With conditional variable, a thread can sleep in a critical section, and
 *  before it sleeps release locks it holds atomically.
 *
 *  wait(lock) atomic(release lock and sleep)
 *  signal() wake up, and re acquire locks.
 *  broadcast() wakeup all waiting threads.
 */

template <typename T> class QueueMon {

  const T *arr;
  std::condition_variable cv;
  Semaphore m;

public:
  QueueMon() : m(1) {}
  void add();
  void remove();
};

template <typename T> void QueueMon<T>::add() {
  m.wait();
  // put item into queue
  cv.notify_one();

  m.signal();
}

template <typename T> void QueueMon<T>::remove() {
  m.wait();
  while (1) {
    // cv.wait();
  }
  m.signal();
}

#endif /* ifndef MONITOR_H */
