#include <stddef.h>
#include <stdlib.h>

typedef struct Bucket {
  int num;
  int *ptrs;
} Bucket;

int* delete_nth(size_t szin, int order[szin], int max_e, size_t *szout) {
  Bucket buffer[szin];
}
