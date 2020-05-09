#include "vec.hpp"
#include "str.hpp"
#include <iostream>

int main(void) {
    Vec<int> v;

    v.push_back(1);
    v.push_back(2);
    v.push_back(2);
    v.push_back(2);
    std::cout << v.size() << std::endl;
    v.clear();
    std::cout << v.size() << std::endl;

    Str s ("asd");
    std::cout << s[2] << std::endl;

    return 0;
}
