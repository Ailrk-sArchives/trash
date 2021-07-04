// https://leetcode.com/problems/linked-list-cycle-ii/

#include <iostream>

using namespace std;

/**
 * Definition for singly-linked list.
 */
struct ListNode {
    int val;
    ListNode *next;
    ListNode(int x) : val(x), next(NULL) {}
};

// TODO proof

// floyd tortoise and hare cycle detection algorithm
// advance pointer with different speed check if they meet.

class Solution {
public:
    ListNode *detectCycle(ListNode *head) {
      ListNode* slow = head;
      ListNode* fast = head;

      // detect the cycle
      do {
        if (!fast || !fast->next) return nullptr;
        fast = fast->next->next;
        slow = slow->next;
      } while (fast != slow);

      // find the cycle
      fast = head;
      while (fast != slow) {
        slow = slow->next;
        fast = fast->next;
      }

      return fast;
    }
};


int main(void)
{
  Solution solution;

  return 0;
}
