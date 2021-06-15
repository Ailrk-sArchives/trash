#include <cassert>
#include <iostream>



struct ListNode {
  int val;
  ListNode *next;
  ListNode() : val(0), next(nullptr) {}
  ListNode(int x) : val(x), next(nullptr) {}
  ListNode(int x, ListNode *next) : val(x), next(next) {}
};

class Solution {
public:
  ListNode *addTwoNumbers_impl(ListNode *l1, ListNode *l2) {
    bool carry = false;
    ListNode *out, *prev = nullptr;

    while (l1 || l2) {
      int vl1 = l1 ?  l1->val : 0;
      int vl2 = l2 ? l2->val : 0;
      int sum = vl1 + vl2;
      assert(vl1 < 10 && vl2 < 10);

      if (carry) {
        sum += 1;
        carry = false;
      }

      if (sum >= 10) {
        carry = true;
        sum -= 10;
      }

      prev = out;
      out = new ListNode(sum);
      out->next = prev;

      if (l1) {
        l1 = l1->next;
      }
      if (l2) {
        l2 = l2->next;
      }
    }

    return reverse(out);
  }

  ListNode *addTwoNumbers(ListNode *l1, ListNode *l2) {
    return addTwoNumbers_impl(reverse(l1), reverse(l2));
  }

  // reverse begin
  //
  ListNode *reverse(ListNode *l) {
    ListNode *next, *prev = nullptr;
    ListNode *cur = l;

    // a -> b -> c
    while (cur != nullptr) {
      next = cur->next;
      cur->next = prev;

      prev = cur;
      cur = next;
    }

    l = prev;
    return l;
  }
};

void free_list(ListNode *l) {

  if (l != nullptr) {
    ListNode *next = l->next;
    delete l;
    free_list(next);
  }

}

void print_list(ListNode *l) {

  while (l) {
    std::cout << l->val << " ";
    l = l->next;
  }
  std::cout << '\n';
}

int main(void) {
  Solution s;

  {
    ListNode *l1 = new ListNode(2, new ListNode(4, new ListNode(3)));

    print_list(l1);

    // be extra careful when freeing pointer returned by destructive algorithm.
    ListNode *l2 = s.reverse(l1);
    print_list(l2);

    free_list(l2);
  }

  {
    ListNode *l1 = new ListNode(2, new ListNode(4, new ListNode(3)));
    ListNode *l2 = new ListNode(5, new ListNode(6, new ListNode(4)));

    ListNode *n = s.addTwoNumbers(l1, l2);
    print_list(n);

    free_list(l1);
    free_list(l2);
    free_list(n);
  }
  return 0;
}
