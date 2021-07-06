#include <iostream>
#include <vector>

using namespace std;

// given an array, find the first k most frequent numbers
//
// Input: nums = [1,1,1,1,2,2,3,4], k = 2
// Output: [1, 2]
//
// steps:
//   use bucket sorts.

class Solution {
public:
    vector<int> topKFrequent(vector<int>& nums, int k) {
    }
};

int main(void)
{
  Solution solution;

  {
    vector<int> v{1, 1, 1, 2, 2, 3};
    int k = 2;
    auto res = solution.topKFrequent(v, k);
    std::cout << res << std::endl;
  }


  {
    vector<int> v{1};
    int k = 1;
    auto res = solution.topKFrequent(v, k);
    std::cout << res << std::endl;
  }

  return 0;
}
