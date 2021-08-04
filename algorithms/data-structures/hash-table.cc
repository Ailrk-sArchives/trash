#include <stdio.h>
#include <cinttypes>

// hash table done in pure c.
// it's so simple that you don't really need anything special.

#define N 1024

uint64_t * lookup(uint64_t k) {
  static uint64_t table[N];

  uint64_t mask = 0xffffffffffffff;
  uint64_t hash = k;
  hash *= 0xcca1cee435c5048f;
  hash ^= hash >> 32;

  for (size_t i = hash % N; ; i = (i + 1) % N) {
    if (!table[i] || (table[i]&mask) == k) {
      return &table[i];
    }
  }
}
