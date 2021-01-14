#include <iostream>
#include <opencv2/core.hpp>
#include <opencv2/imgcodecs.hpp>
#include <opencv2/imgproc.hpp>
#include <opencv4/opencv2/highgui.hpp>
#include <string>

// some basic drawing toolds in opencv.
// use Point to define 2d point in an image.
// use Scalar
// draw line, ellipse, rectangle, circle, filled polygon.

auto myline(cv::Mat &img, const cv::Point start, const cv::Point end) -> void {
  const int thickness = 2;
  const int lineType = 8;
  cv::line(img, start, end, cv::Scalar(0, 0, 0), thickness, lineType);
}
auto myline(cv::Mat &img, const cv::Point start, const cv::Point end,
            const int thickness, const int lineType) -> void {
  cv::line(img, start, end, cv::Scalar(0, 0, 0), thickness, lineType);
}

auto myellipse(cv::Mat &img, const double angle, const int w) -> void {
  const int thickness = 2;
  const int lineType = 8;
  cv::ellipse(img, cv::Point(w / 2.0, w / 2.0), cv::Size(w / 4.0, w / 16.0),
              angle, 0, 360, cv::Scalar(255, 0, 0), thickness, lineType);
}

auto myfilledcirc(cv::Mat &img, const cv::Point center, const int w) -> void {
  const int thickness = 2;
  const int lineType = 8;
  cv::circle(img, center, w / 32.0, cv::Scalar(0, 0, 255), thickness, lineType);
}

auto mypolygon(cv::Mat &img, const int w) -> void {
  const int lineType = 8;
  cv::Point rook_points[20] = {
      cv::Point(w / 4.0, 7 * w / 8.0),
      cv::Point(3 * w / 4.0, 7 * w / 8.0),
      cv::Point(3 * w / 4.0, 13 * w / 16.0),
      cv::Point(11 * w / 16.0, 13 * w / 16.0),
      cv::Point(19 * w / 32.0, 3 * w / 8.0),
      cv::Point(3 * w / 4.0, 3 * w / 8.0),
      cv::Point(3 * w / 4.0, w / 8.0),
      cv::Point(26 * w / 40.0, w / 8.0),
      cv::Point(26 * w / 40.0, w / 4.0),
      cv::Point(22 * w / 40.0, w / 4.0),
      cv::Point(22 * w / 40.0, w / 8.0),
      cv::Point(18 * w / 40.0, w / 8.0),
      cv::Point(18 * w / 40.0, w / 4.0),
      cv::Point(14 * w / 40.0, w / 4.0),
      cv::Point(14 * w / 40.0, w / 8.0),
      cv::Point(w / 4.0, w / 8.0),
      cv::Point(w / 4.0, 3 * w / 8.0),
      cv::Point(13 * w / 32.0, 3 * w / 8.0),
      cv::Point(5 * w / 16.0, 13 * w / 16.0),
      cv::Point(w / 4.0, 13 * w / 16.0),
  };
  const cv::Point *ppt[1] = {rook_points};
  int npt[]{20};
  cv::fillPoly(img, ppt, npt, 1, cv::Scalar(255, 255, 255), lineType);
}

int main(int argc, char *argv[]) {
  const int w = 300;
  std::string atomwindow = "Drawing 1: Atom";
  std::string rookwindow = "Drawing 1: Atom";
  cv::Mat atomimg = cv::Mat::zeros(w, w, CV_8UC3);
  cv::Mat rookimg = cv::Mat::zeros(w, w, CV_8UC3);

  myellipse(atomimg, 90, w);
  myellipse(atomimg, 0, w);
  myellipse(atomimg, 45, w);
  myellipse(atomimg, -45, w);

  myfilledcirc(atomimg, cv::Point(w / 2.0, w / 2.0), w);

  mypolygon(rookimg, w);
  cv::rectangle(rookimg, cv::Point(0, 7 * w / 8.0), cv::Point(w, w),
                cv::Scalar(0, 255, 255), -1, 8);
  myline(rookimg, cv::Point(0, 15 * w / 16), cv::Point(w, 15 * w / 16));
  myline(rookimg, cv::Point(w / 4, 7 * w / 8), cv::Point(w / 4, w));
  myline(rookimg, cv::Point(w / 2, 7 * w / 8), cv::Point(w / 2, w));
  myline(rookimg, cv::Point(3 * w / 4, 7 * w / 8), cv::Point(3 * w / 4, w));

  cv::namedWindow(atomwindow.c_str(), cv::WINDOW_AUTOSIZE);
  cv::namedWindow(rookwindow.c_str(), cv::WINDOW_AUTOSIZE);

  cv::imshow(atomwindow.c_str(), atomimg);
  while (cv::waitKey() != 27)
    ;

  cv::imshow(rookwindow.c_str(), rookimg);
  while (cv::waitKey() != 27)
    ;
  return 0;
}
