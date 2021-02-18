#include <array>
#include <iostream>

// trees are not necessarily to be represented as nodes, you can also use an
// array. it's a much more compact way to represent tree.
//
// Property:
//  1. A binary heap is a complete binary tree,
//  2. Keys has to be in total order.
//       review total order: transitivity, antisymmetry, and connexivity.
//       (connex implies reflexivity)
//
//  The property is based on some total ordered keys.
//
// How do you represent a tree? Use the relative index from the parent to
// find it's children!
//
//         100
//        /   \
//       19    36     smash
//      /  \  /  \      =>   100, 19, 36, 17, 3, 25, 27
//     17  3 25   1
//    / \
//   2   7
//
// operations on binary heap
//  1. insertion O(1)                     | Swim up
//      add to bottom of the tree first.
//      If it's not correct order swap; Recurse until the order is right.
//  2. extract the maximums (delete the root.) O(logn)    | Sink down
//      replace the root with the last element in the  last level.
//      sink the root down if the order is wrong.
//      recurse until the heap property is restored.
//  3. search
//      just as normal binary tree.

template <typename T, typename Ord, size_t Size> class BinHeap {
private:
  std::array<T, Size> data;

public:
  BinHeap() {}

  void insert(T o);
  T extract();
  bool search(T o);
};

