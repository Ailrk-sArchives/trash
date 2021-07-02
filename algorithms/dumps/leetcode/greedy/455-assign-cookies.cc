// https://leetcode.com/problems/assign-cookies/

/* Input: g = [1,2,3], s = [1,1] */
/* Output: 1 */
/* Explanation: You have 3 children and 2 cookies. The greed factors of 3
 * children are 1, 2, 3. */
/* And even though you have 2 cookies, since their size is both 1, you could
 * only make the child whose greed factor is 1 cotent. */
/* You need to output 1.n */

#include <iostream>
#include <vector>

// Allocation problem
// Solution:
//   Sort children and cookies in ascending order. Try to feed the first child
//   with the first cookie, if doesn't work try the next cookie.
//   Each step choose the smallest possible cookie, then eventually we get
//   the smallest amount of cookies needed to feed all children.

using namespace std;

class Solution {
public:
  int findContentChildren(vector<int> &g, vector<int> &s) {
    sort(g.begin(), g.end());
    sort(s.begin(), s.end());
    int gi, si;
    for (gi = 0, si = 0; gi < g.size() && si < s.size(); ++si) {
      if (g[gi] <= s[si]) {
        ++gi;
      }
    }
    return gi;
  }
};


int main(void)
{
  Solution solution;

  {
    vector<int> g {1, 2, 3};
    vector<int> s {1, 1};
    int r1 = solution.findContentChildren(g, s);
    cout << r1 << endl;
  }

  {
    vector<int> g {1, 2};
    vector<int> s {1, 2, 3};
    int r1 = solution.findContentChildren(g, s);
    cout << r1 << endl;
  }


  return 0;
}
