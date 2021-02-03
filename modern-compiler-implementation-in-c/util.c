#include "util.h"
#include <stdio.h>
#include <stdlib.h>

void *checked_malloc(int sz) {
  void *p = malloc(sz);
  if (p == NULL) {
    fprintf(stderr, "Memory error\n");
    exit(-1);
  }
  return p;
}
