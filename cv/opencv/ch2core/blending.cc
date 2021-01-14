#include "../common.h"
#include <opencv2/highgui.hpp>
#include <opencv2/imgcodecs.hpp>

// linear blend operator
// g(x) = (1 - alpha) * f0(x) + alpha * f1(x)
// where 1 > alpha > 0

int main(int argc, char *argv[]) {
  double alpha = 0.5;
  double beta, input;

  cv::Mat src1, src2, dst;
  std::string imgname1;
  std::string imgname2;

  std::cout << "Simple linear blender" << std::endl;
  std::cout << "---------------------" << std::endl;
  std::cout << "Enter alpha [0 - 1]  " << std::endl;
  std::cin >> input;
  std::cout << "Enter file1:  " << std::endl;
  std::cin >> imgname1;
  std::cout << "Enter file2:  " << std::endl;
  std::cin >> imgname2;

  if (input >= 0.0 && input <= 1.0)
    alpha = input;

  // needs to be the same size
  src1 = cv::imread(imgname1.c_str());
  src2 = cv::imread(imgname2.c_str());

  cv::namedWindow("Linear Blend", 1);

  // generate g(x) image.
  beta = { 1.0 - alpha };
  // calculate weighted sum of two arrays.
  cv::addWeighted(src1, alpha, src2, beta, 0.0, dst);

  cv::imshow("Linear Blend", dst);

  return 0;
}


