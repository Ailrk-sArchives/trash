#include <stdlib.h>

static int isodd(int v) {
  return v & 0x1;
}

int find_outlier(const int *values, size_t count) {
  const int flag = isodd(values[0]);
  for (size_t i = 1; i < count; i++) {
    if (flag ^ isodd(values[i])) {
      if (i == 1 && (flag ^ isodd(values[i + 1])))
        return values[0];
      return values[i];
    }
  }
  return 0;
}

int main(void) { return 0; }
