// low context switching spin lock.
// native version 1

#include <benchmark/benchmark.h>

#include <atomic>
#include <cstdint>
#include <iostream>
#include <thread>
#include <vector>

class spin_lock {
private:
  // make sure read-modify-write of the flag is atomic;
  std::atomic<bool> locked{false};
};
