#include <atomic>
#include <chrono>
#include <iostream>
#include <thread>
#include <vector>

// what do system people do.?

// Four cases to demonstrate the effect of sharing cacheline
// has on performance.

void work(std::atomic<int> &a) {
  for (int i = 0; i < 100000; i++) {
    a++;
  }
}

void single_thread() {
  std::atomic<int> a;
  a = 0;

  for (int i = 0; i < 3; ++i) {
    work(a);
  }
}

// direct sharing.
//  a will bounce around in four different cachelines.
void same_var() {
  std::atomic<int> a;
  a = 0;

  std::vector<std::thread> ts{};

  for (int i = 0; i < 3; ++i) {
    ts.emplace_back(std::thread([&]() { work(a); }));
  }

  for (auto &t : ts) {
    t.join();
  }
}

// false sharing.
// although different threads are using different atomic values,
// they happen to align in the sme cache line.
// the same cacheline will also bouncing around four threads, but this
// time 3/4 are useless.
void diff_var() {
  std::atomic<int> a;
  std::atomic<int> b;
  std::atomic<int> c;
  std::atomic<int> d;

  std::thread t1([&]() { work(a); });
  std::thread t2([&]() { work(b); });
  std::thread t3([&]() { work(c); });
  std::thread t4([&]() { work(d); });

  t1.join();
  t2.join();
  t3.join();
  t4.join();
}

// No sharing at all among 4 threads.
// cacheline has maximum size 64 bytes. if we align the struct
// to 64 we can guarantee two structs don't fit in one cachline.
struct alignas(64) AlignedType {
  AlignedType() { val = 0; }
  std::atomic<int> val;
};

void diff_line() {
  AlignedType a{};
  AlignedType b{};
  AlignedType c{};
  AlignedType d{};

  std::thread t1([&]() { work(a.val); });
  std::thread t2([&]() { work(b.val); });
  std::thread t3([&]() { work(c.val); });
  std::thread t4([&]() { work(d.val); });

  t1.join();
  t2.join();
  t3.join();
  t4.join();
}

#define BENCHMARK(fn)                                                          \
  {                                                                            \
    auto start = std::chrono::high_resolution_clock::now();                    \
    fn();                                                                      \
    auto end = std::chrono::high_resolution_clock::now();                      \
    std::chrono::duration<double, std::milli> elapsed = end - start;           \
    std::cout << #fn ": " << elapsed.count() << "ms" << std::endl;             \
  }

int main(void) {

  BENCHMARK(single_thread);
  BENCHMARK(same_var);
  BENCHMARK(diff_var);
  BENCHMARK(diff_line);

  return 0;
}
