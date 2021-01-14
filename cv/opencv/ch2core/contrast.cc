#include <iostream>
#include <opencv2/core.hpp>
#include <opencv2/highgui.hpp>
#include <opencv2/imgcodecs.hpp>

// general image processing operator:
//  a function take one or more input images and produces an output image.
//  image transformation can be seen as
//  Point operator. (pixel transform) like brightness and contrast adjustment
//  neightborhood operator. (area based)

// brightness and contrast adjustment
//  g(x) = alpha * f(x) + beta
//    where alpha > 0, beta is called gain and bias.

int main(int argc, char *argv[]) {
  double alpha; // contrast control
  int beta;     // bias, brightness control
  cv::Mat image = cv::imread(argv[1]);
  cv::Mat newimg = cv::Mat::zeros(image.size(), image.type());

  std::cout << "Basic Linear tranforms" << std::endl;
  std::cout << "----------------------" << std::endl;
  std::cout << "Enter alpha value:";
  std::cin >> alpha;
  std::cout << "Enter beta value:";
  std::cin >> beta;

  // traversal through every pixels. and assign new value.
  for (int y = 0; y < image.rows; ++y) {
    for (int x = 0; x < image.cols; ++x) {
      for (int c = 0; c < 3; ++c) {
        auto current = newimg.at<cv::Vec3b>(y, x)[c];
        current = cv::saturate_cast<uchar>(alpha * current + beta);
      }
    }
  }
  cv::namedWindow("Og", 1);
  cv::namedWindow("New", 1);

  cv::imshow("Og", image);
  cv::imshow("New", newimg);
  while (cv::waitKey() != 27)
    ;

  return 0;
}
