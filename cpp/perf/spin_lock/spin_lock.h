// active backoff

#include <benchmark/benchmark.h>

#include <atomic>
#include <cstdint>
#include <iostream>
#include <thread>
#include <vector>

class spin_lock {
protected:
  std::atomic<bool> locked{false};

public:
  void lock();

  void unlock() { locked.store(false); } // @write
};

class scoped_spin_lock {
private:
  spin_lock &lk;

public:
  scoped_spin_lock(spin_lock &lk) : lk(lk) { lk.lock(); }
  ~scoped_spin_lock() { lk.unlock(); }
};

inline void inc(spin_lock &s, std::int64_t value) {

  for (int i = 0; i < 100000; ++i) {
    scoped_spin_lock lk{s};
    value++; // @write
  }
}

template <typename Lock> inline void spin_lock_bench(benchmark::State &s) {
  auto num_threads = s.range(0);

  std::int64_t value = 0;

  std::vector<std::thread> threads;
  threads.reserve(num_threads);

  Lock slk{};

  for (auto _ : s) {
    for (auto i = 0u; i < num_threads; ++i) {
      threads.emplace_back([&] { inc(slk, value); });
    }

    for (auto &t : threads) {
      t.join();
    }

    threads.clear();
  }
}
