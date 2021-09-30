#include <iostream>
#include <queue>
#include <vector>

#define PRINT(pq, v)                                                           \
    for (auto &n : v) {                                                        \
        auto e = pq.top();                                                     \
        std::cout << e << " ";                                                 \
        pq.pop();                                                              \
    }                                                                          \
    std::cout << std::endl;

template <typename T> struct comp {
    bool operator()(T a, T b) { return a < b; }
};

void con1() {
    std::priority_queue<int, std::vector<int>, comp<int>> pq;

    std::vector<int> v{ 5, 3, 1, 6, 4, 8, 2 };
    for (auto &n : v) {
        pq.push(n);
    }
    PRINT(pq, v);
}

void con2() {
    std::vector<int> v{ 5, 3, 1, 6, 4, 8, 2 };
    auto cp = [](auto a, auto b) { return a < b; };
    std::priority_queue<int, std::vector<int>, decltype(cp)> pq(cp);
    std::cout << "1" << std::endl;
    for (auto &n : v) {
        pq.push(n);
    }
    PRINT(pq, v);
}

int main(void) {
    con1();
    con2();
    return 0;
}
