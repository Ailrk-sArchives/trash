#include <iostream>
#include <iterator>
#include <algorithm>
#include <vector>

void istream_iter_func() {
    std::cout << "istream iterator demo: ..." << std::endl;
    std::vector<int> v;
    std::copy(std::istream_iterator<int>(std::cin),
              std::istream_iterator<int>(),
              std::back_inserter(v));

    for (auto e : v)
        std::cout << e << std::endl;
}


void ostream_iter_func() {
    std::cout << "ostream iterator demo: ..." << std::endl;
    std::vector<int> v {9, 8, 7, 6, 5};
    std::copy(v.begin(),
              v.end(),
              std::ostream_iterator<int>(std::cout, ">>="));
    std::cout << std::endl;
}


int main(void) {
    // istream iterator.
    istream_iter_func();
    ostream_iter_func();
    return 0;
}
