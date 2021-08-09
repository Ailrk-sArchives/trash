#include <iostream>
#include <opencv4/opencv2/core.hpp>
#include <opencv4/opencv2/highgui.hpp>
#include <opencv4/opencv2/imgcodecs.hpp>
#include <opencv4/opencv2/imgproc.hpp>

int main(int argc, char *argv[]) {
  char *imageName = argv[1];
  cv::Mat image;
  image = cv::imread(imageName, 1);
  if (argc != 2 || !image.data) {
    printf("No image data\n");
    return -1;
  }

  cv::Mat gray_image;
  cv::cvtColor(image, gray_image, cv::COLOR_BGR2GRAY);
  cv::imwrite("~/newDisk/img/coins.png", gray_image);
  cv::namedWindow(imageName, cv::WINDOW_AUTOSIZE);
  cv::namedWindow("Gray image", cv::WINDOW_AUTOSIZE);

  cv::imshow(imageName, image);
  cv::imshow("Gray image", gray_image);

  while (int key = (cv::waitKey()) != 27)
    ; // 27 for esc
  return 0;
}
