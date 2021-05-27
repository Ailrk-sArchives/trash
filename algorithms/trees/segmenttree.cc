#include <concepts>
#include <functional>
#include <iostream>
#include <memory>
#include <optional>
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
// 0-0  1-1 12 13   14
//  |    |
//  10  11

template <typename C, typename Operator> class SegTree {
private:
  std::vector<typename C::value_type> tree_;
  std::vector<bool> lazy_;
  C data_;
  Operator op;

  void build(typename C::iterator s, typename C::iterator t, size_t p) {
    if (s == t) {
      tree_[p] = *s;
      return;
    }

    auto m = s + ((t - s) >> 1);
    build(s, m, p * 2 + 1);
    build(m + 1, t, p * 2 + 2);

    tree_[p] = op(tree_[p * 2 + 1], tree_[p * 2 + 2]);
  }

public:
  const C &data() { return data_; }

  SegTree(const C &data, Operator op)
      : op(op), data_(data), tree_(4 * data.size()), lazy_(false, data.size()) {

    // note this is a quirk when using iterator as pointer. end is 1 over the
    // last element.
    build(data_.begin(), data_.end() - 1, 0);
  }

  SegTree(C &&data, Operator op)
      : op(op), data_(std::move(data)), tree_(4 * data.size()),
        lazy_(false, data.size()) {
    build(data_->begin(), data_->end() - 1, 0);
  }

  SegTree(SegTree &&seg) = default;
  SegTree(const SegTree &seg) = default;

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

  // range query.
  std::optional<typename C::value_type>
  get_interval_in(auto left, auto right, auto s, auto t, size_t p) {

    static auto update_sum = [this](auto &v, auto &sum) {
      if (v.has_value() && sum.has_value()) {
        sum.emplace(op(v.value(), sum.value()));
      } else if (v.has_value()) {
        sum.emplace(v.value());
      }
    };

    // [left, right] is the search range.
    // [s, t] is the current range.

    if (left <= s && t <= right) {
      return {tree_[p]};
    }

    std::optional<typename C::value_type> sum, v1, v2;
    auto m = s + ((t - s) >> 1);

    if (left <= m) {
      v1 = get_interval_in(left, right, s, m, p * 2 + 1);
      update_sum(v1, sum);
    }
    if (right > m) {
      v2 = get_interval_in(left, right, m + 1, t, p * 2 + 2);
      update_sum(v2, sum);
    }
    return sum;
  }

  typename C::value_type get_interval(typename C::iterator left,
                                      typename C::iterator right) {
    return get_interval_in(left, right, data_.begin(), std::prev(data_.end()),
                           0)
        .value();
  }

  typename C::value_type get_interval(size_t left, size_t right) {
    return get_interval_in(data_.begin() + left, data_.begin() + right,
                           data_.begin(), std::prev(data_.end()), 0)
        .value();
  }

  // range update with lazy propagation.
  void update_in(auto left, auto right, auto f, auto s, auto t) {
    // TODO
  }

  void update(typename C::iterator left, typename C::iterator right, auto f) {
    return update_in(left, right, f, data_.begin(), std::prev(data_.end()));
  }

  void update(size_t left, size_t right, auto f) {
    return update_in(data_.begin() + left, data_.begin() + right, f,
                     data_.begin(), std::prev(data_.end()));
  }

#ifdef TEST
  void dump() {
    std::cout << "Dumping..." << std::endl;
    std::cout << "data: " << std::endl;
    for (auto &v : data_) {
      std::cout << v << " ";
    }
    std::cout << "\n";
    std::cout << "tree: " << std::endl;
    for (auto &v : tree_) {
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

  {
    std::cout << "== interval test ==" << std::endl;
    std::vector<int> data1{10, 11, 12, 13, 14};
    SegTree seg1{data1, [](auto a, auto b) { return a + b; }};
    seg1.dump();

    // check all ranges.
    for (int i = 0; i < data1.size(); ++i) {
      for (int j = i; j < data1.size(); ++j) {
        auto v = seg1.get_interval(i, j);
        std::cout << "(" << i << ", " << j << "): " << v << " " << std::endl;
      }
    }
  }

  return 0;
}
