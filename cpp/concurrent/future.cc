#include <future>
#include <iostream>
#include <mutex>
#include <numeric>
#include <thread>
#include <vector>

// https://en.wikipedia.org/wiki/Futures_and_promises
//
// C++ async mechanism:
// An async operation has two side: [the producer/wr] and [the consumer]
//
// - Why promise and future are separate?
//   1. std::promise is a writable pending value (the function set the value),
//      std::future is a readony pending value (the value).
//
//      future can be defiend without specifying which pormise to set its value.
//      the value of a future can be set by any promise, but it can only be set once.
//
//      Settting the value of a future is called resolving/fulfilling.
//
// - What's the motivation of promise and future if we have thread already?
//   promise based model makes concurrent code easier to write.
//
//   futures and promise decouple the computation and the return value of the computation,
//   You can refer to the result of the computation as future and use freely without
//   concern how the value is computeted.
//
//   If the value is computed, where the value is used can preceed. Otherwise the thread
//   will wait for the value to arrive.
//
//   Doing so we can avoid writing too many low level locks. Essentially get put a lock
//   on the value acces it iself.
//
// - What does it mean by having more flexibility on computation (decouple computation and result)?
//   If you have a future, you can use it as other normal values.
//
//   Usually in an imperative setting, if you are using a value, it assumes some computation related
//   to this value is done before you are using it. In another word, the existence of the value
//   is part of the precondition of where you use it.
//
//   If you wrap the computation in a promise and use it's result as future, it's not necessarily
//   true that the value is computed when you are using it. The related computation is in promise
//   and is invoked when needed, a bit like lazy eval.
//
//   But because the whole computation is captured by the promise, it doesn't matter how you execute
//   it. You can run the code in promise synchronously, asynchornously, in another thread, in anther
//   core, or even somewhere far far away.
//
//   With promies and future, all you need to ensure is when future.get() is called, the computation
//   is done somewhere and the valueeventually arives. The flexibility comes by the fact that the
//   computation is decoupled from the current control flow.
//
// - Is it necessary for future and promise be two separate things?
//   No, you can totally create a promise by specifying what computation should it run,
//   and what value it should return, and use the exact same promise as the pending value.
//   It's just happen C++ stl design promose like this.
//   Now you not only have lower level primitives like promise + thread, you also can built
//   async which is a higher level interface.
//
// - Is c++ async model CSP? (communitaion sequential process)
//
//

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

  void bar(const std::string &str) {
    std::lock_guard<std::mutex> lk(*m);
    std::cout << str << std::endl;
  }

  std::string operator()(int i, const std::string &str) {
    std::lock_guard<std::mutex> lk(*m);
    std::cout << str << " " << i << std::endl;
    return "DONE: " + str;
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

  std::cout << "\n";
  {
    // use async to run parallel...
    std::vector<int> v(10000, 2);
    std::cout << "Sum is: " << parallel_sum(v.begin(), v.end()) << std::endl;
  }

  std::cout << "\n";
  {
    std::shared_ptr<std::mutex> m = std::make_shared<std::mutex>();
    X x{m};
    // (&a)->foo(42, "hello");
    auto a1 = std::async(&X::foo, &x, 42, "a1: hello, I maybe async or deferred");

    // x.bar("world") in lazy
    auto a2 =
        std::async(std::launch::deferred, &X::bar, x, "a2: world!, I'm deferred");

    // X()(43) in async
    auto a3 =
        std::async(std::launch::async, X{m}, 43, "a3: I'm in another thread!");
    std::cout << a3.get() << std::endl;

    //  cannot get twice
    // std::cout << a3.get() << std::endl;

    a2.wait(); // lazy eval
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
