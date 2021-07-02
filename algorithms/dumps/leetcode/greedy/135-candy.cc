// https://leetcode.com/problems/candy/

#include <iostream>
#include <vector>
using namespace std;

// Another allocation algorithm
// Proof
//   Sort make sure list in ascending order.
//   Each step checks if the current rating is the same as the one before it.
//   If it is, award same amount of candy, otherwise award one more.

class Solution {
public:
    int candy(vector<int>& ratings) {
      sort(ratings.begin(), ratings.end());
      int total_candies = 0;
      int next_award = 1;
      for (int i = 0; i < ratings.size(); ++i) {
        if (i - 1 < 0) {
          total_candies += 1;
        } else {
          if (ratings[i - 1] < ratings[i]) {
            next_award += 1;
          }

          total_candies += next_award;
        }
      }

      return total_candies;
    }
};


int main() {

  Solution solution;

  {
    vector<int> ratings {1, 0, 2};
    int res = solution.candy(ratings);
    std::cout << res << std::endl;

  }

  return 0;
}
