#include <functional>
#include <iostream>
#include <opencv2/core.hpp>
#include <opencv2/highgui.hpp>
#include <opencv2/imgcodecs.hpp>
#include <opencv2/imgproc.hpp>
#include <variant>

// different blur algorithms.
// gaussian filter, median filter, bilateral filter.
// smoothing can reduce noise and help further image progessing.

// linear filter (a common type of filter)
//    g(i, j) = Sum(f(i+k, j+k) * h(k, l)) where h(k, l) is the kernel.

// some common blurring filteres
// average filter, gaussian filter,  median filter
bool display_dst(int delay, cv::Mat &dst, const std::string &windowname);

template <typename T>
static inline auto apply(cv::Mat &src, cv::Mat &dst,
                         const int max_kernal_length, const T &filter,
                         const std::string &windowname) -> bool {
  for (int i = 1; i < max_kernal_length; i = i + 2) {
    if constexpr (std::is_same_v<T, decltype(cv::blur)>) {
      // size(w, h): define size of kernel
      // point(-1, -1): indicate position of anchor point. negative indicate
      // center of kernal.
      static_cast<decltype(cv::blur)>(filter)(src, dst, cv::Size(i, i),
                                              cv::Point(-1, -1));

    } else if constexpr (std::is_same_v<T, decltype(cv::GaussianBlur)>) {
      // size(w, h): size of kernal. w, h has to be odd.
      // seg_x std dev in x. = imply kernal size.
      // seg_y std dev in x. = imply kernal size.
      static_cast<decltype(cv::GaussianBlur) *>(filter)(src, dst,
                                                        cv::Size(i, i), 0, 0);

    } else if constexpr (std::is_same_v<T, decltype(cv::medianBlur)>) {
      static_cast<decltype(cv::medianBlur) *>(filter)(
          src, dst, i); // i size of the kernal. must be odd.

    } else if constexpr (std::is_same_v<T, decltype(cv::bilateralFilter)>) {
      // d: parameter of each pixel neighbour.
      // seg_color: standard dev in color space
      // seg_space: std dev in coordinate space (in pixel term)
      static_cast<decltype(cv::bilateralFilter)>(filter)(src, dst, i, i * 2,
                                                         i / 2);
    }

    if (!display_dst(1500, dst, windowname))
      return false;
  }

  return true;
}

bool display_caption(const std::string &caption, cv::Mat &src,
                     const std::string &windowname) {
  cv::Mat dst = cv::Mat::zeros(src.size(), src.type());
  cv::putText(dst, caption, cv::Point(src.cols / 4, src.rows / 2),
              cv::FONT_HERSHEY_COMPLEX, 1, cv::Scalar(255, 255, 255));
  cv::imshow(windowname, dst);
  int c = cv::waitKey(1500);
  if (c >= 0) {
    return false;
  }

  return true;
}

bool display_dst(int delay, cv::Mat &dst, const std::string &windowname) {
  cv::imshow(windowname, dst);
  int c = cv::waitKey(1500);
  if (c >= 0) {
    return false;
  }

  return true;
}

int main(int argc, char *argv[]) {
  constexpr int kernel_lenght = 31;
  cv::Mat src, dst;
  std::string windowname = "Filter demo";
  cv::namedWindow(windowname, cv::WINDOW_AUTOSIZE);
  if (argc != 2) {
    std::cout << "not enough parameter" << std::endl;
    std::cout << "usage: " << std::endl;
    std::cout << argv[0] << " <image file>" << std::endl;
    return -1;
  }

  std::string imgname = argv[1];
  src = cv::imread(imgname, 1);

  if (!display_caption("OG", src, windowname))
    return 0;

  apply(src, dst, kernel_lenght, cv::blur, windowname);
  apply(src, dst, kernel_lenght, cv::GaussianBlur, windowname);
  apply(src, dst, kernel_lenght, cv::medianBlur, windowname);
  apply(src, dst, kernel_lenght, cv::bilateralFilter, windowname);

  return 0;
}
