// scan images, lookup tables and time measurement
#include <array>
#include <iostream>
#include <opencv2/core.hpp>
#include <opencv2/imgcodecs.hpp>
#include <string_view>
#include "../common.h"

// 1. color space reduction.
// (255, 255, 255) scheme for color pixel sometimes can be hard to compute,
// and the precision of colors for some algorithms is not critical.
// thus we can reduce the color space to (I/n) * n where n is number of levels.

// 2. lookup table for optimization.

static void help() {
  std::cout << "\n-----------------------------" << std::endl
            << "Usage: " << std::endl
            << "scan <image file> <int> [G] " << std::endl
            << "\n-----------------------------" << std::endl;
}

cv::Mat scanImageAndReduceC(cv::Mat &I, const std::array<uchar, 256> table);
cv::Mat scanImageAndReduceIterator(cv::Mat &I,
                                   const std::array<uchar, 256> table);
cv::Mat scanImageAndReduceRandomAccess(cv::Mat &I,
                                       const std::array<uchar, 256> table);

// calcualte lookup table.
// motivation
// Rather than apply (divideWidth * (i / divideWidth)) to every pixels,
// caclulate the mapping table ahead of time and just
// replace pixels with value in table.
static inline auto tableLoopUp(int divideWidth) -> std::array<uchar, 256> {
  std::array<uchar, 256> table;
  double t;
  for (int i = 0; i < 256; ++i) {
    table[i] = (uchar)(divideWidth * (i / divideWidth));
  }
  return table;
}

int main(int argc, char *argv[]) {
  help();
  if (argc < 3) {
    std::cout << "Not enougth paramters" << std::endl;
    return -1;
  }
  cv::Mat I, J;
  if (argc == 4 && !std::strcmp(argv[3], "G"))
    I = cv::imread(argv[1], cv::IMREAD_GRAYSCALE);
  else
    I = cv::imread(argv[1], cv::IMREAD_COLOR);

  if (!I.data) { // pointer to the data.
    std::cout << "Image" << argv[1] << " could not be loaded." << std::endl;
    return -1;
  }

  // read divideWidth.
  int divideWidth = 0;
  std::stringstream s;
  s << argv[2];
  s >> divideWidth;

  std::array<uchar, 256> table = tableLoopUp(divideWidth);

  // benchmarking.
  benchmark(
      100,
      [&I, &J, &table]() {
        cv::Mat cloneI = I.clone();
        J = scanImageAndReduceIterator(cloneI, table);
      },
      "reducing with iterator");

  benchmark(
      100,
      [&I, &J, &table]() {
        cv::Mat cloneI = I.clone();
        J = scanImageAndReduceC(cloneI, table);
      },
      "reducing with C operator");

  benchmark(
      100,
      [&I, &J, &table]() {
        cv::Mat cloneI = I.clone();
        J = scanImageAndReduceRandomAccess(cloneI, table);
      },
      "reducing with on the fly address generatoin");

  benchmark(
      100,
      [&I, &J, &table]() {
        cv::Mat lookupTable(1, 256, CV_8U);
        uchar *p = lookupTable.data;
        for (size_t i = 0; i < 256; ++i)
          p[i] = table[i];
        cv::LUT(I, lookupTable, J);
      },
      "use core function LUO");

  return 0;
}

// with C style access. fast
auto scanImageAndReduceC(cv::Mat &I, const std::array<uchar, 256> table)
    -> cv::Mat {
  CV_Assert(I.depth() == CV_8U);     // only accept char type matrices.
  const int channels = I.channels(); // get number of matrix channels.
  size_t nCols = I.cols * channels;  // get actual amount of col.
  size_t nRows = I.rows;

  if (I.isContinuous()) { // check if Mat is stored in a countinous mem.
    nCols *= nRows;       // if it is just loop through the memory.
    nRows = 1;
  }

  // will be 1 if image is stored countinously.
  for (size_t i = 0; i < nRows; ++i) {
    uchar *p = I.ptr(i);
    for (size_t j = 0; j < nCols; ++j) {
      p[j] = table[p[j]]; // assign the precaculated reduce space.
    }
  }

  return I;
}

// safer way of doing it with iterator.
// you don't need to care if image is stored continously or not.
// but since it uses iterator speed might get compensated.
cv::Mat scanImageAndReduceIterator(cv::Mat &I,
                                   const std::array<uchar, 256> table) {
  CV_Assert(I.depth() == CV_8U);
  const int channels = I.channels();

  switch (channels) {
  case 1: {
    for (auto it = I.begin<uchar>(), end = I.end<uchar>(); it != end; ++it) {
      *it = table[*it];
    }
    break;
  }
  case 3: {
    // cv::Vec3b is the short vector for uchar type.
    // openCV iterator go through columns and automatically skip the next row.
    for (auto it = I.begin<cv::Vec3b>(), end = I.end<cv::Vec3b>(); it != end;
         ++it) {
      (*it)[0] = table[(*it)[0]];
      (*it)[1] = table[(*it)[1]];
      (*it)[2] = table[(*it)[2]];
    }
    break;
  }
  }
  return I;
}

// on the fly address calcualtion with reference returning.
cv::Mat scanImageAndReduceRandomAccess(cv::Mat &I,
                                       const std::array<uchar, 256> table) {
  CV_Assert(I.depth() == CV_8U);
  const int channels = I.channels();
  switch (channels) {
  case 1: {
    for (size_t i = 0; i < I.rows; ++i)
      for (size_t j = 0; j < I.cols; ++j)
        I.at<uchar>(i, j) = table[I.at<uchar>(i, j)];
    break;
  }
  case 3: {
    // get copied. Mat_ take type parameter of type to look for.
    cv::Mat_<cv::Vec3b> _I = I;
    for (size_t i = 0; i < I.rows; ++i)
      for (size_t j = 0; j < I.cols; ++j) {
        // calculate address based on type and coord on fly.
        _I(i, j)[0] = table[_I(i, j)[0]];
        _I(i, j)[1] = table[_I(i, j)[1]];
        _I(i, j)[2] = table[_I(i, j)[2]];
      }
    I = _I;
    break;
  }
  }
  return I;
}
