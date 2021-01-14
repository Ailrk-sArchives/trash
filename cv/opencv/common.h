#include <string_view>
#include <iostream>
#include <functional>
#include <opencv2/core.hpp>

static inline void benchmark(const int times, std::function<void()> &&f,
                             const std::string_view report) {
  // benchmarak
  double t;
  t = (double)cv::getTickCount();
  for (int i = 0; i < times; ++i) {
    f();
  }
  t = 1000 * ((double)cv::getTickCount() - t) / cv::getTickFrequency();
  t /= times; // get sec.
  std::cout << report << ": " << t << " ms." << std::endl;
}

