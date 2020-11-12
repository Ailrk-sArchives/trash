
#include <iostream>
#include <thread>

#include "semaphore.h"

void too_much() {
  Semaphore m;
  bool no_milk = true;
  auto buy_milk = [&](std::string_view name) {
    m.wait();
    if (no_milk) {
      std::cout << "Buying milk from thread 1" << std::endl;
      no_milk = false;
    }
    m.signal();
  };

  auto t1 = std::thread(buy_milk, "thread 1");
  auto t2 = std::thread(buy_milk, "thread 2");

  t1.join();
  t2.join();
}
