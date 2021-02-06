#include <iostream>
#include <thread>
#include <vector>

// thread is introduced in c++11.
// it provides utilities not only starting and managing
// threads. But also synchronization primitives.

int main(void) {
  std::vector<std::thread> threads;

  std::thread detached([]() {
    std::cout << "I am detached from: " << std::this_thread::get_id()
              << std::endl;
  });
  // calling detached make this thread execute indepenently
  // from the main thread.
  // So if main thread finished eailer, it's not necessary
  // finished yet.
  detached.detach();

  for (int i = 0; i < 10; ++i

  ) {
    threads.push_back(std::thread([]() {
      std::cout << "hi from " << std::this_thread::get_id() << std::endl;
    }));
  }

  for (auto &t : threads) {
    // calling join force the current thread
    // waiting until t1 is finished.

    t.join();
  }

  return 0;
}
