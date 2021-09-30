#include <algorithm>
#include <iostream>
#include <iterator>
#include <vector>

template <typename T>
std::vector<int>::iterator current(std::reverse_iterator<T> ri) {
    return (ri).base();
}

// notice base iterator points one after the reverse iterator.
// when converting back,
// (p.rbegin()+1).base == p.end(),
// (p.rend()).base == p.begin
int main(void) {
    int i[] = { 1, 2, 3, 4 };
    std::vector<int> nums(i, i + 4);

    auto reverse_iter_1 = std::make_reverse_iterator(nums.begin());
    auto reverse_iter = std::make_reverse_iterator(nums.end());

    auto forward_iter = current(reverse_iter_1);

    std::cout << *nums.begin() << std::endl;
    std::cout << *nums.rbegin() << std::endl;
    std::cout << *(nums.rbegin() + 1).base() << std::endl;

    std::cout << "--" << std::endl;
    std::cout << *nums.end() << std::endl;
    std::cout << *(nums.end() - 1) << std::endl;

    // call a.rbegin().base will get on after
    // what current reverse iter points to.
    // x     _     _      _      _  reverse
    //
    //       ^
    //     rend+1
    //
    // _     _     _      _       x  base
    // ^
    // begin
    // which is rend + 1
    std::cout << *(nums.rend() - 1) << std::endl;

    std::cout << *(nums.rend() + 1).base() << std::endl;

    std::cout << "index with reverse iterator" << std::endl;
    auto riter = nums.rbegin();
    riter++;
    riter++; // iter in i[1]
    std::cout << nums.size() - 1 - (riter - nums.rbegin()) << std::endl;

    std::cout << "--" << std::endl;
    std::for_each_n(nums.begin(), 4, [](int n) { std::cout << n << " "; });

    std::cout << "--" << std::endl;
    std::for_each_n(reverse_iter, 4, [](int n) { std::cout << n << " "; });

    std::cout << "--" << std::endl;
    std::for_each_n(forward_iter, 4, [](int n) { std::cout << n << " "; });

    return 0;
}
