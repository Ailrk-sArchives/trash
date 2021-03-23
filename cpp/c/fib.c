#include <stdio.h>

static int num = 10;

int main(void) {
  int i, cur, next;
  int tmp;
  // here it shows the difference between lisp do.
  // in a lisp do, each interation will always use the
  // older value, but in C for loop, the side effect will
  // happen right in place.
  // if you write cur = next, all statement follow that will
  // use cur as next.
  for (i = cur = 0, next = 1; i < num;
       ++i, tmp = cur, cur = next, next = next + tmp) {
    printf("%d ", i);
    printf("%d ", cur);
    printf("%d \n", next);
  }
  return 0;
}
