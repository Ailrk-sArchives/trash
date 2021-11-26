
#include <iostream>
#include <type_traits>

class A {
    int type;
};

class B : A {
    int type = 3;
    int val1;
    int val2;
};

int main(void)
{

    static_assert(sizeof(A) == 4);

    // the redifinition of type just hide the old one in the base class.
    static_assert(sizeof(B) != 12);
    static_assert(sizeof(B) == 16);

    return 0;
}
