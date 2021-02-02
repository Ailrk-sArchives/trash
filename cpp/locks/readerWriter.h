/*
 * Reader Writer problem:
 *  - each read or write of the shared data must happend within a
 *    critical section.
 *  - Guarantee mutual exlucsion for writers.
 *  - Allow multiple readers to execute in the critical section at once.
 */

#include "semaphore.h"

class ReadWrite {
  int readers;
  Semaphore mutex; // control access to readers
  Semaphore wrt;   // control entry to first writer or reader.
public:
  explicit ReadWrite(int readers, int m, int w)
      : readers(readers), mutex(m), wrt(w) {}
  void read();
  void write();
};

void ReadWrite::write() {
  wrt.wait(); // any writers or readers?
  // do write
  wrt.signal();
}

void ReadWrite::read() {
  mutex.wait(); // ensure mutual exclusion
  readers += 1; // another reader
  if (readers == 1)
    wrt.wait(); // block writers.
  mutex.signal();

  // perform read

  mutex.wait(); // ensure mutual exclusion again.
  readers -= 1;
  if (readers == 0)
    wrt.signal();
  mutex.signal();
}
