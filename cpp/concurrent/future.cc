#include <future>
#include <iostream>
#include <mutex>
#include <numeric>
#include <thread>
#include <vector>

// std::future provides mechanism to access the result of asynchronous
// operations. What is an asynchronous operation?
//   operations created by
//    1. std::async
//    2. std::packaged_task
//    3. std::promise

namespace promise_demo {
//// std::promise
//   Promise-future communication channel:
//
//   Each promise associates with a shared state and a result.
//   Operations on shared state:
//   1. *make ready* (unlock critical section allows waiting threads come in)
//   2. *release* (gives up its reference to the shared state.)
//      references to shared state act in a reference counting scheme.
//      If there is no reference to the shared state, the state is destroyed.
//   3. *abandon* store exception >> make ready >> realease.
//

// fold + on the vector,
// set value of promise to be the result,
// then signal promise to be ready.
//
void accumulate(std::vector<int>::iterator first,
                std::vector<int>::iterator last,
                std::promise<int> acc_promise) {
  int sum = std::accumulate(first, last, 0);

  std::cout << "\nsetting in accumulate: " << std::endl;
  // setting value and notifing the future.
  acc_promise.set_value(sum);
}

// sleep to simulate work,
// set barrier promise with no value. This will only signal it to be ready.
void do_work(std::promise<void> barrier) {
  std::this_thread::sleep_for(std::chrono::seconds(1));
  barrier.set_value();
}

void promise_demo() {
  std::cout << "Promise demo: ====" << std::endl;
  std::vector<int> numbers{1, 2, 3, 4, 5, 6};

  // create the promise
  std::promise<int> acc_promise;

  // get the future of the promise:
  // The result of promise is obtained from the future.
  // Once promise is finished future will have the value.
  std::future<int> acc_future = acc_promise.get_future();

  // NOTE this won't work. Different from std::thread, std::promise will not
  // automatically spawn a thread. Instead it merely represent a pending
  // computation that can have a pending return value future.
  //
  // std::cout << "result=" << acc_future.get() << std::endl;

  // launch the promie in a thread.
  // thread constructor has loads of overloads.
  std::thread worker(accumulate, numbers.begin(), numbers.end(),
                     std::move(acc_promise));

  // future::get() to get the result from promise. In this case is the sum of
  // vectors we calcualted.
  // It will wait until the future has a valid result.
  // Doesn't need to call wait before get.
  std::cout << "result=" << acc_future.get() << std::endl;
  worker.join();

  {
    // use promise<void> to signal state between threads.
    std::promise<void> barrier;
    std::future<void> barrier_future = barrier.get_future();
    std::thread new_worker(do_work, std::move(barrier));
    barrier_future.wait();
    new_worker.join();
  }

  std::cout << "End promise demo: ====" << std::endl;
}

} // namespace promise_demo

namespace async_demo {
// std::async funs the function asynchronously, and return a future to represent
// the pending result.

// There are two modes (execution policy):
// 1. std::launch::async (run function in a separate thread)
// 2. std::launch::deferred (lazy eval the function, call f synchronously)

struct X {
  std::shared_ptr<std::mutex> m;
  X(std::shared_ptr<std::mutex> m) : m(m) {}

  void foo(int i, const std::string &str) {
    std::lock_guard<std::mutex> lk(*m);
    std::cout << str << " " << i << std::endl;
  }

  void bar(const std::string *str) {
    std::lock_guard<std::mutex> lk(*m);
    std::cout << str << std::endl;
  }

  int operator()(int i) {
    std::lock_guard<std::mutex> lk(*m);
    std::cout << i << std::endl;
    return i + 10;
  }
};

// sum an vector in parallel.
// Takes any iterator.
// split it in half.
//
// recursively sum the first half and second half, but
// put the second half in anothe thread.

template <typename RandomIt> int parallel_sum(RandomIt first, RandomIt last) {
  auto len = last - first;
  if (len < 1000) {
    return std::accumulate(first, last, 0);
  }

  RandomIt mid = first + len / 2;
  auto handle =
      std::async(std::launch::async, parallel_sum<RandomIt>, mid, last);
  int sum = parallel_sum(first, mid);
  return sum + handle.get();
}

void async_demo() {
  std::cout << "Future demo: ===" << std::endl;
  auto future = std::async(std::launch::async,
                           []() { std::cout << "I am a thread" << std::endl; });
  future.get(); // retrieve the value from future.

  {
    std::vector<int> v(10000, 2);
    std::cout << "Sum is: " << parallel_sum(v.begin(), v.end()) << std::endl;
  }

  std::cout << "End future demo: ===" << std::endl;
}

} // namespace async_demo

//// std::packaged_task

int main(void) {
  async_demo::async_demo();
  std::cout << "\n";
  promise_demo::promise_demo();

  return 0;
}
