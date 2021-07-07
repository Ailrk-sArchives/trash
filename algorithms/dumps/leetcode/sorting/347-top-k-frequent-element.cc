#include <iostream>
#include <queue>
#include <unordered_map>
#include <vector>

using namespace std;
using namespace std;
template <typename C> void print_seq(C &&container) {
  for (auto &n : container) {
    std::cout << n << " ";
  }
  std::cout << std::endl;
}

// given an array, find the first k most frequent numbers
//
// Input: nums = [1,1,1,1,2,2,3,4], k = 2
// Output: [1, 2]
//
// steps:
//   use bucket sorts.

class Solution {
public:
  vector<int> topKFrequent(vector<int> &nums, int k) {
    std::vector<int> result;
    std::unordered_map<int, int> counts;
    for (const auto &n : nums) {
      ++counts[n];
    }

    auto comp = [](const auto &a, const auto &b) {
      return a.second > b.second;
    };

    std::priority_queue<std::pair<int, int>,
                        std::vector<std::pair<int, int>, decltype(comp)>>
        q(comp);

    for (const auto &t : counts) {
      q.push(t);
    }

    while (k >= 1) {
      auto &[n, v] = q.top();
      q.pop();
      result.push_back(n);
      k--;
    }

    return result;
  }
};

int main(void) {
  Solution solution;

  {
    vector<int> v{1, 1, 1, 2, 2, 3};
    int k = 2;
    auto res = solution.topKFrequent(v, k);
    print_seq(res);
  }

  {
    vector<int> v{1};
    int k = 1;
    auto res = solution.topKFrequent(v, k);
    print_seq(res);
  }

  return 0;
}
