#include <iostream>
#include <vector>

template <typename T> class Vec : public std::vector<T> {
  public:
    using std::vector<T>::vector;
    T pop1() {
        T val(std::move(this->back()));
        this->pop_back();
        return val;
    }
};

struct Bomb {
    int x;
    Bomb(int n)
        : x(n) {}
    Bomb(Bomb &&bomb) { if (bomb.x == 0) { throw; } x = bomb.x; }
    Bomb(Bomb const &bomb) { if (bomb.x == 0) { throw; } x = bomb.x; }
};

int main(void) {
    Vec<int> v1{ 1, 2, 3 };
    auto n = v1.pop1();
    std::cout << n << std::endl;
    Vec<Bomb> v2{ 2, 1, 0, 3 };
    std::cout << v2.pop1().x << std::endl;
    std::cout << v2.pop1().x << std::endl;
    std::cout << v2.pop1().x << std::endl;
    std::cout << v2.pop1().x << std::endl;
    return 0;
}
