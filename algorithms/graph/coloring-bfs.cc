// A different way of thinking about breath first search.
// 1. white: default for all nodes at the beginning
// 2. black: nodes that are not connected with white
// 3. gray: the remainnig (connected both black and white)
//
// We can think using queue as a way to manage those gray nodes.
//
//      b
//     / \
//    g   g
//   / \   \
//   w w    w

#include <iostream>
#include <unordered_map>
#include <vector>

template <typename T> using Graph = std::unordered_map<std::string, T>;


