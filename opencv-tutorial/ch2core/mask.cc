#include <iostream>
#include <opencv4/opencv2/core.hpp>

auto sharpen(const cv::Mat &img) -> cv::Mat;

int main(int argc, char *argv[]) { return 0; }

// mask
//          | 0   -1   0 |
//  I = I * | -1   5  -1 |
//          | 0   -1   0 |
auto sharpen(const cv::Mat &img) -> cv::Mat {
  CV_Assert(img.depth() == CV_8U);
  cv::Mat result;
  result.create(img.size(), img.type());  // output image.
  const int nchannels = img.channels();

  for (int j = 1; j < img.rows; ++j) {    // get row
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
