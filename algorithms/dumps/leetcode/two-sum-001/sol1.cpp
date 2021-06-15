#include <unordered_map>
#include <vector>

using namespace std;

// Idea:
// Use values in nums as index of a map
//

class Solution {

public:
// twoSum begin
// Proof of correctness:
//    Initial:
//      Set up a map M so it's keys are values in the input vector, and
//      values are indices of values in the vector.
//      Set up a empty vector V to accumulate the final result.
//    Maintain:
//      Traverse over the input vector. In each iteration i we have
//      t = target,
//      n = nums[i] be the value in nums[i],
//      we have e = t - n to be the expected value.
//      If m = M[e] exits and e + n = t,
//      Then nums[m] + nums[i] = t,
//      The first time we find m, we push m and i into V and break out of
//      the loop.
//    Final:
//      Because we break out of the loop immediately after we find the pair,
//      len(V) = 2.
//      Because input guarantee there exists unique pair of indicies that satisifies
//      the condition, we find all solutions.

  vector<int> twoSum(vector<int> &nums, int target) {
    unordered_map<int, int> mapping;
    vector<int> result;

    for (int i = 0; i < nums.size(); ++i) {
      mapping[nums[i]] = i;
    }

    for (int i = 0; i < nums.size(); ++i) {
      int another = target - nums[i];
      if (mapping.count(another) && mapping[another] != i) {
        result.push_back(mapping[another]);
        result.push_back(i);
        break;
      }
    }
    return result;
  }

// Complexity Analysis:
// Time O(n):
//   We tracerse over the list twice, and the hash table look up takes
//   O(1) time.
//
// Space O(n):
//   O(n). We need extra space for hashmap to store the indices, which
//   forces us to store at least n exra elements.
// twoSum end
};
