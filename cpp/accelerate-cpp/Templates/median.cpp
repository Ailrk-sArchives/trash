#include <stdexcept>
#include <vector>
#include <algorithm>
#include <iostream>

template <typename T>
T median(std::vector<T> v) {
    typedef typename std::vector<T>::size_type vec_size;

    vec_size size = v.size();
    if (size == 0)
        throw std::domain_error("median of an empty vector");

    std::sort(v.begin(), v.end());
    vec_size mid = size / 2;

    return size % 2 == 0 ? (v[mid] + v[mid + 1]) / 2 : v[mid];
}

int main(void) {
    std::cout << median(std::vector<int> {1,2,3,4,5,5,5,5,6,6})
        << std::endl;

    return 0;
}
