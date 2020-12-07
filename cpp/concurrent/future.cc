#include <future>
#include <iostream>
#include <thread>

void future_demo() {
  auto future = std::async(std::launch::async,
                           []() { std::cout << "I am a thread" << std::endl; });

  future.get();
}

int main(void)
{
  future_demo();

  return 0;
}
