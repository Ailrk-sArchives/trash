#include <iostream>

// underlying implementation
template <typename T1, typename T2> void func(T1 e1, T2 e2) {
  std::cout << "called" << std::endl;
}

// now we want this function to accept all different references.
// These are all overloads you need to provide
template <typename T1, typename T2> void wrapper(T1 &e1, T2 &e2) {
  func(e1, e2);
}

template <typename T1, typename T2> void wrapper(const T1 &e1, T2 &e2) {
  func(e1, e2);
}

template <typename T1, typename T2> void wrapper(T1 &e1, const T2 &e2) {
  func(e1, e2);
}

template <typename T1, typename T2> void wrapper(const T1 &e1, const T2 &e2) {
  func(e1, e2);
}

template <typename T1, typename T2> void wrapper(T1 &e1, T2 &&e2) {
  func(e1, e2);
}

template <typename T1, typename T2> void wrapper(T1 &&e1, T2 &e2) {
  func(e1, e2);
}

template <typename T1, typename T2> void wrapper(T1 &&e1, T2 &&e2) {
  func(e1, e2);
}

template <typename T1, typename T2> void wrapper(const T1 &e1, T2 &&e2) {
  func(e1, e2);
}

template <typename T1, typename T2> void wrapper(T1 &&e1, const T2 &e2) {
  func(e1, e2);
}

// to solve this problem we use perfect forwarding
// it takes the advantage of reference collapsing
template <typename T1, typename T2> void good_wrapper(T1 &&e1, T2 &&e2) {
  func(std::forward<T1>(e1), std::forward<T2>(e2));
}
