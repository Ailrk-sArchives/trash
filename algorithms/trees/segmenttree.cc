#include <concepts>
#include <functional>
#include <iostream>
#include <memory>
#include <random>
#include <vector>

#define TEST

// Segment tree can be used to maintain information of intervals.
// O(logN) modify single element, modify range, and query in intervals.
// Segment tree is a static data structure, it can't be modified once it's
// built.
// the time complexity to build a segment tree: O(nlogn).
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
//    If represent the tree with an array, let the root node be d[1], then we
//    have left child d[2i], right child d[2i+1].
//
//  Space:
//    let n be number of leaves.
//    binary tree      =>    height = log(n)
//                     =>    # of leaves = 2^(log(n))
//    full binary tree =>    total # of nodes = 2^(log(n) + 1) - 1
//
//  imperical value for n: set length = 4n
//          0-4
//        /    \
//      0-2     3-4
//     /  \     / \
//   0-1   2-2 3-3 4-4
//   /  \   |   |   |
// 0-0  1-1 12 12   13
//  |    |
//  10  11

template <typename C, typename Operator> class SegTree {
private:
  std::unique_ptr<std::vector<typename C::value_type>> tree;
  std::unique_ptr<C> data;
  Operator op;

  void build(typename C::iterator s, typename C::iterator t, size_t p) {
    if (s == t) {
      tree->at(p) = *s;
      return;
    }

    auto m = s + (static_cast<int>(t - s) >> 1);
    build(s, m, p * 2 + 1);
    build(m + 1, t, p * 2 + 2);

    tree->at(p) = op(tree->at(p * 2 + 1), tree->at(p * 2 + 2));
  }

public:
  SegTree(const C &data, Operator op)
      : op(op), data(std::make_unique<C>(data)),
        tree(std::make_unique<std::vector<typename C::value_type>>(
            4 * data.size())) {
    // note this is a quirk when using iterator as pointer. end is 1 over the
    // last element.
    build(this->data->begin(), this->data->end() - 1, 0);
  }

  SegTree(C &&data, Operator op)
      : op(op), data(std::make_unique<C>(std::move(data))),
        tree(std::make_unique<std::vector<typename C::value_type>>(
            4 * data.size())) {
    build(this->data->begin(), this->data->end() - 1, 0);
  }

  SegTree(SegTree &&seg) {
    data = seg.data;
    tree = seg.tree;
    seg.data = nullptr;
    seg.tree = nullptr;
  }

  SegTree(const SegTree &seg) {
    this->tree =
        std::make_unique<std::vector<typename C::value_type>>(*seg.tree);
    this->data = std::make_unique<C>(*seg.data);
  }

  SegTree &operator=(const SegTree &seg) {
    SegTree tmp{seg};
    if (this != &seg) {
      tmp.swap(*this);
    }
    return *this;
  }

  SegTree &operator=(SegTree &&seg) {
    if (this != &seg) {
      *this = std::move(seg);
    }
    return *this;
  }

  void swap(SegTree &seg) { std::swap(*this, seg); }

  typename C::value_type get_interval(typename C::iterator first, typename C::iterator last) {

  }

#ifdef TEST
  void dump() {
    std::cout << "Dumping..." << std::endl;
    std::cout << "data: " << std::endl;
    for (auto &v : *data) {
      std::cout << v << " ";
    }
    std::cout << "\n";
    std::cout << "tree: " << std::endl;
    for (auto &v : *tree) {
      std::cout << v << " ";
    }
    std::cout << "\n";
  }
#endif
};

int main(void) {

  {

    std::vector<int> data1{10, 11, 12, 13, 14};
    SegTree seg1{data1, [](auto a, auto b) { return a + b; }};
    seg1.dump();

    SegTree seg2 = seg1;
    seg2.dump();

    SegTree seg3{data1, [](auto a, auto b) { return a * b; }};
    seg3.dump();
  }
  {
    std::vector<int> data;

    std::random_device rng;
    std::mt19937 gen(rng());
    std::uniform_int_distribution<int> dist;

    for (int i = 0; i < 100; ++i) {
      auto value = abs(dist(gen)) % 100;
      data.push_back(value);
    }

    SegTree seg{data, [](auto a, auto b) { return a + b; }};
    seg.dump();
  }

  return 0;
}
