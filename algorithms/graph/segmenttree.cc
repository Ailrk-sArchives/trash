#include <array>
#include <iostream>

// Segment tree can be used to maintain information of intervals.
// O(logN) modify single element, modify range, and query in intervals.
// Segment tree is a static data structure, it can't be modified once it's
// built.
//
// To build a segment tree the time complexity is O(nlogn).
//
// Idea:
// what is interval? A interval is like an edge with two vertices.
// Let S be a set of intervals, and p1, p2, ... pn be endpoints of each
// intervals consecutively. We have intervals in S as:
// (-inf, p1), (p1, p1), (p1, p2), (p2, p2)...(pm, inf)
//
// This intervals can be constructred as a tree with follwoing properties:
//  1. full binary tree.
//  2. leaves represent points.
//  3. internal nodes are folded value of intervals.
//
// Analysis:
//  Index:
//    If represent the tree with an array, let the root node be d[1], then we have
//    left child d[2i], right child d[2i+1].
//
//  Space:
//    let n be number of leaves.
//    binary tree      =>    height = log(n)
//                     =>    # of leaves = 2^(log(n))
//    full binary tree =>    total # of nodes = 2^(log(n) + 1) - 1
//
//  imperical value for n: set length = 4n


template <typename T, size_t Size> class SegTree {
private:
  std::array<T, Size> tree;
  void pushdown();

public:
  SegTree(const std::array<T, Size> &tree) {}
};
