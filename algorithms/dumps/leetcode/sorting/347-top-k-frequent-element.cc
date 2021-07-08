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

    std::unordered_map<int, int> counts;
    for (auto &n : nums) {
      ++counts[n];
    }

    auto comp = [](auto a, auto b) { return a.second < b.second; };

    std::priority_queue<std::pair<int, int>, std::vector<std::pair<int, int>>,
                        decltype(comp)>
        pq(comp);

    for (auto &n : counts) {
      pq.push(n);
    }

    std::vector<int> result;

    while (k >= 1) {
      result.emplace_back(pq.top().first);
      pq.pop();
      --k;
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

  {
    vector<int> v{2, 1, 1, 3, 2, 2, 2, 1};
    int k = 1;
    auto res = solution.topKFrequent(v, k);
    print_seq(res);
  }

  return 0;
}
