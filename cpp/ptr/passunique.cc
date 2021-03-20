#include <iostream>
#include <memory>
#include <random>
#include <thread>

int seed_offset = 0;
// get a rference of the pointer
template <typename T> void side_effect_with_ref(T &ptr) { (*ptr)++; }

template <typename T> void side_effect_with_copy(T ptr) { (*ptr)++; }

template <typename T> void side_effect_with_move(T &&ptr) { (*ptr)++; }
// pass the pointer by value directly.
template <typename T> void side_effect_with_ptr(T *ptr) { (*ptr)++; }

template <typename T> void side_effect_with_ref_to_val(T &val) {
  std::mt19937 gen(123 + seed_offset);
  seed_offset++;
  std::uniform_int_distribution<int> dist(0, 100);
  if (dist(gen) % 2 != 0) {
    std::cout << std::this_thread::get_id() << " incrementing.." << std::endl;
    val++;
  } else {
    std::cout << std::this_thread::get_id() << " decrementing.." << std::endl;
    val--;
  }
}

void naive() {
  auto ptr = std::make_unique<int>(10);
  std::cout << "first: " << *ptr << std::endl;
  side_effect_with_ref(ptr);
  side_effect_with_ref_to_val(*ptr);
  side_effect_with_ref_to_val(*ptr);
  side_effect_with_ref_to_val(*ptr);

  std::cout << "second: " << *ptr << std::endl;
}

void pass_unique_ptr() {
  auto ptr = std::make_unique<int>(10);
  std::cout << "first: " << *ptr << std::endl;

  // filthy
  // std::shared_ptr<int> sptr{std::shared_ptr<int>(ptr.get())};
  // side_effect_with_copy(sptr);

  side_effect_with_ptr(ptr.get());

  side_effect_with_move(std::move(ptr));

  std::cout << "ptr after moved: " << *ptr << std::endl;
}

void multiple_writer() {
  auto ptr = std::make_unique<int>(10);
  std::cout << "first: " << *ptr << std::endl;

  auto thread1 = std::thread([&ptr]() {
    std::cout << "from" << std::this_thread::get_id() << std::endl;
    side_effect_with_ref_to_val(*ptr);
  });
  auto thread2 = std::thread([&ptr]() {
    std::cout << "from" << std::this_thread::get_id() << std::endl;
    side_effect_with_ref_to_val(*ptr);
  });
  auto thread3 = std::thread([&ptr]() {
    std::cout << "from" << std::this_thread::get_id() << std::endl;
    side_effect_with_ref_to_val(*ptr);
  });

  thread1.join();
  thread2.join();
  thread3.join();
  std::cout << "ptr second: " << *ptr << std::endl;
}

int main(void) {
  naive();
  std::cout << "===============================" << std::endl;
  pass_unique_ptr();
  std::cout << "===============================" << std::endl;
  multiple_writer();
  std::cout << "===============================" << std::endl;
  return 0;
}
