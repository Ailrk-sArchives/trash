// https://leetcode.com/problems/non-overlapping-intervals/

#include <iostream>
#include <tuple>
#include <vector>

using namespace std;


// Problem:
//   input of intervals [begin, end]. how to remove intervals to make all
//   intervals in the vector non overlapping
// Steps:
//   1. sort base on the end value.
//   2. traverse over all begins, for
//      interval_i = [begin_i, end_i], interval_j = [begin_j, end_j],
//      let current_incterval = interval_I
//      if begin_j > end_i, if so, remove interval_j
//      else current_inverval = interval_j and go for the next iteration
//
// Proof:
//   Because it's sorted base on starting interval:
//    1. if removing the next interval that begins within the previous interval
//       guarantee an overlap,
//    2. if we don't need to remove the next interval we can guarantee there
//       is no overlap up to current interval.
//    By indcution once we repeat the process for the whole vector we have
//    no overlap.
//

class Solution {
public:
    int eraseOverlapIntervals(vector<vector<int>>& intervals) {
      sort(intervals.begin(), intervals.end(), [](auto x, auto y) {
          return x[1] < y[1];
          });
      int num = 0;
      int prev = intervals[0][1];

      for (int i = 1; i < intervals.size(); ++i) {
        if (intervals[i][0] < prev) {
          ++num;
        } else {
          prev = intervals[i][1];
        }
      }

      return num;
    }
};

int main(void)
{
  Solution solution;
  {
    vector<vector<int>> v{{1, 2}, {2, 3}, {3, 4}, {1, 3}};
    auto r = solution.eraseOverlapIntervals(v);
    std::cout << r << std::endl;
  }

  return 0;
}
