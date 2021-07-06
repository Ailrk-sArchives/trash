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

// floyd tortoise and hare cycle detection algorithm
// advance pointer with different speed check if they meet.
//
// 1. use a fast pointer that skip 2 nodes per step, and a slow pointer that
//    skip one element per step.
// 2. if fast reach the end, there is no loop.
// 3. if fast and slow pointers meet, there is a loop
// 4. the distance from where fast and slow pointer meet to the start of the
//    loop is the distance from the starting point to the start of the loop.
//    [lemma] leave slow pointer where it is, put fast pointer  at the
//            beginning of the list, advance both pointers one step at a time
//            eventually they meet at the start of the loop.

class Solution {
public:
  ListNode *detectCycle(ListNode *head) {
    ListNode *slow = head;
    ListNode *fast = head;

    // detect loop
    do {
      if (!fast || !fast->next)
        return nullptr;
      fast = fast->next->next;
      slow = slow->next;
    } while (fast != slow);

    // find the cycle.

    fast = head;
    while (fast != slow) {
      fast = fast->next;
      slow = slow->next;
    }

    return fast;
  }
};

int main(void) {
  Solution solution;

  return 0;
}
