// https://leetcode.com/problems/candy/

#include <iostream>
#include <vector>
#include <numeric>
using namespace std;

// Another allocation algorithm
// Problem:
//   1. Make sure child with higher rating get more candies then it's neighbour,
//   2. Make sure total candies usage is minimum.
//
// Steps:
//   1. make sure all child have at least one candy.
//   2. make sure each child with higher rating in the left hand size has one
//      more candy.
//      traverse from left to right,
//      update rating by comparing current child with it's left neighbour.
//      if current child has higher rating, it needs to have at least
//      one more candy than it's left child.
//   3. make sure each child with higher rating in the right hand side  has one
//      more candy.
//      traverse from right to left, if left child's rating is not graeter then
//      current, update left child = right child + 1
//
// Proof:
//   P1: by traverse from left to right, we ensure all elements with higher
//       rating then it's left have at least one more candy.
//       by traverse from right to left, we ensure all elements with higher
//       rating then it's right has at least one more candy.
//
//       In case two consecutive elements in a row, we only need to increment
//       once.

class Solution {
public:
    int candy(vector<int>& ratings) {
      int size = ratings.size();
      if (size < 2) {
        return size;
      }

      // initialize vector with size n to 1 never use initializer list...
      vector<int> candies(size, 1);

      for (int i = 1; i < size; ++i) {
        if (ratings[i] > ratings[i - 1]) {
          candies[i] = candies[i -  1] + 1;
        }
      }

      for (int i = size - 1; i > 0; --i) {
        if (ratings[i] < ratings[i - 1]) {
          candies[i - 1] = max(candies[i - 1], candies[i] + 1);
        }
      }

      return accumulate(candies.begin(), candies.end(), 0);
    }
};

int main() {

  Solution solution;

  {
    vector<int> ratings {1, 0, 2};
    int res = solution.candy(ratings);
    std::cout << res << std::endl;
  }

  {
    vector<int> ratings {1, 2, 2};
    int res = solution.candy(ratings);
    std::cout << res << std::endl;
  }


  return 0;
}
