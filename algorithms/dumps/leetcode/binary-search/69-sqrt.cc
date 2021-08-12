// https://leetcode.com/problems/sqrtx/

#include <iostream>


class Solution {
public:
    int mySqrt(int x) {
      if (x == 0) return x;
      int l = 1;
      int r = x;

      while (l <= r) {
        int mid = (l + (r - 1)) / 2;
        int sqrt = x / mid;
        if (sqrt == mid) {
          return mid;
        } else if (mid > sqrt) {
          r = mid - 1;
        } else {
          l = mid + 1;
        }
      }
      return r;
    }

    // with newton iteration method
    int mySqrt1(int x) {
      long s = x;
      while (s * s > x) {
        s = (s + x / s) / 2;
      }
      return s;
    }
};
