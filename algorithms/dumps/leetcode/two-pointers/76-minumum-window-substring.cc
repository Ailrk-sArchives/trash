// https://leetcode.com/jummy233/

#include <iostream>
#include <vector>
#include <string>

using namespace std;

// Input: s = "ADOBECODEBANC", t = "ABC"
// Output: "BANC"

// Problem:
//   two string S, T find the length of the shortest substring of S
//   that contains all characters in T.
//
// sliding window for range search

class Solution {
public:
    string minWindow(string s, string t) {
      vector<bool> flag (128, false);
      vector<int> chars(128, 0);

      int min_l = 0, min_r = s.size() + 1;

      // load info from the search string
      for (int i = 0; i < t.size(); ++i) {
        ++chars[t[i]];
        flag[t[i]] = true;
      }

      // scan the string
      for (int count = 0, l = 0, r = 0; r < s.size(); ++r) {

        if (flag[s[r]]) {
          if (--chars[s[r]] >= 0) ++count;
        }

        while (count == t.size()) {
          if (r - l + 1 < min_r) {
            min_l = l;
            min_r = r - l + 1;
          }
          if (flag[s[l]] && ++chars[s[l]] > 0)
            --count;
          ++l;
        }
      }
      return min_r > s.size() ? "" : s.substr(min_l, min_r);
    }
};

int main(void)
{

  Solution solution;

  {
    string s{"ADDBEC0DEBANC"};
    string t{"ABC"};

    string res = solution.minWindow(s, t);
    std::cout << res << std::endl;
  }
  return 0;
}
