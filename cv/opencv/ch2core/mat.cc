#include <opencv4/opencv2/core.hpp>
#include <iostream>

int main(int argc, char *argv[]) {
  // cv::Mat is a customized reference count pointer.
  // maybe implemented with a shared pointer?
  // it has .clone() and .copyTo() to copy the matrix memory underneath.
  // Mat allows to use different color space and data type.
  // color space: [grayscale, rgb (bgr), hsv, hls]

  // cv::Scalar is a four element short vector. specify initialize data.
  cv::Mat m1(2, 2, CV_8UC3, cv::Scalar(0, 0, 255));

  // use arrays to initialize Mat.
  // m2(dimension, size of each dimension, ...)
  // Mat with dim > 2 cannot be printed (of course...)
  int sz[3] = {2, 2, 2};
  cv::Mat m2(3, sz, CV_8UC(1), cv::Scalar::all(0));

  // matlab style matrix initializer.
  cv::Mat m3 = cv::Mat::eye(4, 4, CV_64F);
  cv::Mat m4 = cv::Mat::ones(2, 2, CV_32F);
  cv::Mat m5 = cv::Mat::zeros(3, 3, CV_8UC1);

  // small matrix
  // use MatrixComma initializer
  // some template stuffs.
  cv::Mat m6 = (cv::Mat_<double>(3, 3) << 0, -1, 0, -1, 5, -1, 0, -1, 0);

  cv::Mat m7 = m6.clone();

  std::cout << "M1=" << m1 << std::endl;
  return 0;
}
