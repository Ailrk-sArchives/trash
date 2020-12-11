#include <iostream>
#include <mutex>
#include <string>
#include <thread>

// Thead safe singleton.
// Creational design pattern.

// Ensure sinlge point of access to it.
class Singleton {
  static std::mutex mutex_;
  static Singleton *singleton_;

  std::string value = "default";
  Singleton() {}

public:
  // You should not be able to create another singleton.
  Singleton(const Singleton &other) = delete;
  Singleton &operator=(const Singleton &other) = delete;

  // the only access poing of the content.
  static Singleton *get_instance();

  const std::string &get_value() const { return value; }
  void set_value(const std::string &str) {
    std::cout << "setting..." << std::endl;
    singleton_->value = str;
  }
};

// define static member outside the clasa
Singleton *Singleton::singleton_ = nullptr;
std::mutex Singleton::mutex_;

// Race condition alert.
// Note, if two threads both calling the get_instance
// for the first time at the same time, there will be
// two Singletons get created.
// That's why we lock the call here.
Singleton *Singleton::get_instance() {
  std::lock_guard<std::mutex> lg(mutex_);
  if (singleton_ == nullptr) {
    std::cout << "oh" << std::endl;
    singleton_ = new Singleton();
  }
  return singleton_;
}

int main(void) {
  auto t1 = std::thread([]() {
    std::this_thread::sleep_for(std::chrono::milliseconds(0));
    Singleton *singleton = Singleton::get_instance();
    singleton->set_value("gogo");
    std::cout << singleton->get_value() << std::endl;
  });

  auto t2 = std::thread([]() {
    std::this_thread::sleep_for(std::chrono::milliseconds(0));
    Singleton *singleton = Singleton::get_instance();
    singleton->set_value("baba");
    std::cout << singleton->get_value() << std::endl;
  });

  t1.join();
  t2.join();

  Singleton::get_instance()->set_value("done!");
  std::cout << Singleton::get_instance()->get_value() << std::endl;
  return 0;
}
