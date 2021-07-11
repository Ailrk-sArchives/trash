#include <cassert>
#include <iostream>

//   0 0 1 0
// ^ 0 1 0 1
// ----------
//   0 1 1 1
//
// x xor y (x ^ y):
//   return 1 when either x or y is 1, otherwise return 0
//   in another word, return 1 whenever x and y are differetn
// use case:
//   1. use xor to toggle bit
//   2. use xor to get positions in two numbers that are different

class Solution {
public:
  int hammingDistance(int x, int y) {
    int difference = x ^ y;
    int n = 0;
    while (difference) {
      n += difference & 1;
      difference >>= 1;
    }
    return n;
  }
};

int main() {
  Solution solution;

  {
    int x = 0b001, y = 0b100;
    int res = solution.hammingDistance(x, y);
    std::cout << res << std::endl;
    assert(res == 2);
  }
}
