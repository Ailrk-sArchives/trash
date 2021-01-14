#include "../common.h"
#include <iostream>
#include <opencv2/core.hpp>
#include <opencv2/highgui.hpp>
#include <opencv2/imgcodecs.hpp>
#include <opencv2/imgproc.hpp>
#include <string>

auto sharpen(const cv::Mat &img) -> cv::Mat;

auto help(std::string_view progName) -> void {
  std::cout << "Usage: " << std::endl
            << progName << " [image_name] [G -- grayscale]" << std::endl;
}

int main(int argc, char *argv[]) {
  help(argv[0]);
  if (argc < 2) {
    std::cerr << "Not enought parameter" << std::endl;
  }
  std::string filname{argv[1]};

  cv::Mat I, J, K;
  if (argc >= 3 && !std::strcmp("G", argv[2]))
    I = cv::imread(filname.c_str(), cv::IMREAD_GRAYSCALE);
  else
    I = cv::imread(filname.c_str(), cv::IMREAD_COLOR);

  cv::namedWindow("Input", cv::WINDOW_AUTOSIZE);
  cv::namedWindow("Output", cv::WINDOW_AUTOSIZE);

  cv::imshow("Input", I);

  cv::Mat Isharpen1, Isharpen2;
  benchmark(
      1, [&I, &Isharpen1]() { Isharpen1 = sharpen(I); },
      "sharpening use formula methods");

  // use cv::Mat_ to make matrix with specific shape.
  // filter2D to apply a filter to an image.
  benchmark(
      1,
      [&I, &Isharpen2]() {
        cv::Mat kern = (cv::Mat_<char>(3, 3) << 0, -1, 0, -1, 5, -1, 0, -1, 0);
        cv::filter2D(I, Isharpen2, I.depth(), kern);
      },
      "sharpening with kernal");

  cv::imshow("Output", Isharpen1);
  while (int key = (cv::waitKey()) != 27)
    ;

  cv::imshow("Output", Isharpen2);
  while (int key = (cv::waitKey()) != 27)
    ;

  return 0;
}

// mask
//          | 0   -1   0 |
//  I = I * | -1   5  -1 |
//          | 0   -1   0 |
auto sharpen(const cv::Mat &img) -> cv::Mat {
  CV_Assert(img.depth() == CV_8U);
  cv::Mat result;
  result.create(img.size(), img.type()); // output image.
  const int nchannels = img.channels();

  for (int j = 1; j < img.rows; ++j) { // get row
    const uchar *prev = img.ptr<uchar>(j - 1);
    const uchar *current = img.ptr<uchar>(j);
    const uchar *next = img.ptr<uchar>(j + 1);
    uchar *ouput = result.ptr(j);

    // traversal columns.
    for (int i = nchannels; i < nchannels * (img.cols - 1); ++i) {
      *ouput++ =
          cv::saturate_cast<uchar>(5 * current[i] - current[i - nchannels] -
                                   current[i + nchannels] - prev[i] - next[i]);
    }
  }

  // skip the border by setting it to 0.
  result.row(0).setTo(cv::Scalar(0));
  result.row(result.rows - 1).setTo(cv::Scalar(0));
  result.col(0).setTo(cv::Scalar(0));
  result.col(result.cols - 1).setTo(cv::Scalar(0));
  return result;
}
