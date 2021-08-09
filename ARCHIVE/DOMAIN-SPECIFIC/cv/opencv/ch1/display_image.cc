#include <iostream>
#include <opencv2/core.hpp>
#include <opencv2/highgui.hpp>
#include <opencv2/imgcodecs.hpp>

// core.hpp basic building blocks of the lib
// highgui functions for input and output operations.

int main(int argc, char *argv[]) {
  if (argc != 2) {
    std::cout << "Usage: display_image ImageToLoadAndDisplay" << std::endl;
  }

  cv::Mat image;
  image = cv::imread(argv[1]);

  if (!image.data) {
    std::cout << "Could not open or find the image" << std::endl;
  }

  cv::namedWindow("Display window", cv::WINDOW_AUTOSIZE);
  cv::imshow("Display window", image);
  while (int key = (cv::waitKey()) != 27)
    ; // 27 for esc
  return 0;
}
