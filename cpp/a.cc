#include <iostream>
#include <type_traits>
struct A {
    int x;
};

struct C {
    C() {}
    C(C const &c) { std::cout << "copy" << std::endl; }
    C(C &&c) { std::cout << "move" << std::endl; }

    friend std::ostream &operator<<(std::ostream &os, C const &c) {
        os << "c";
        return os;
    }
};

template <typename T> void foo(T &&value) {
    // static_assert(sizeof(T) == -1);
    value.x++;
    std::cout << "good" << std::endl;
}

template <typename T> void bar(T &&value) {
    // static_assert(sizeof(T) == -1);
    std::cout << value << std::endl;
}

template <typename T> struct AA {

    template <typename Z, typename std::enable_if_t<
                              std::is_convertible_v<std::remove_cvref_t<Z>,
                                                    std::remove_cvref_t<T>>,
                              bool> = true>
    void bar1(Z &&value) {
        std::cout << value << std::endl;
    }
};

int main() {
    A a;
    A &aref = a;
    foo(aref);
    for (int i = 0; i < 10; ++i) {
        bar(i);
    }

    {

        AA<int> aa;
        for (int i = 0; i < 10; ++i) {
            aa.bar1(i);
        }

        for (int i = 0; i < 10; ++i) {
            int const j = i;
            aa.bar1(j);
        }

        for (int i = 0; i < 10; ++i) {
            aa.bar1(10);
        }
    }

    {
        AA<C> aa;
        for (int i = 0; i < 10; ++i) {
            C c;
            aa.bar1(c);
        }
        std::cout << "--" << std::endl;

        for (int i = 0; i < 10; ++i) {
            C c;
            C &cref = c;
            aa.bar1(cref);
        }
        std::cout << "--" << std::endl;

        for (int i = 0; i < 10; ++i) {
            aa.bar1(C());
        }

        std::cout << "--" << std::endl;
        for (int i = 0; i < 10; ++i) {
            C c;
            aa.bar1(std::move(c));
        }
        std::cout << "--" << std::endl;
    }
}
