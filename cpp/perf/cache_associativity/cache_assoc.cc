#include <iostream>
#include <benchmark/benchmark.h>

// cache design
// - Direct Map Cache
//   concecutive bins. An address will map to a particular bin.
//   | | | | | - len = 4
//    0 1 2 3  - idicies
// which bin a memory go? idx <- addr `mod` len
//
// - Fully associated Cache
//   | ...  ... | - we can shove cache block in any one of the bin.
//
// - Set associative cache.
//   Combine both DM and FA.
//          way0  way1
//   set 0 |    |    |
//   set 1 |    |    |
//
//   1. mod to get set idx
//   2. find empty slot and save the cache block there.


BENCHMARK_MAIN();
