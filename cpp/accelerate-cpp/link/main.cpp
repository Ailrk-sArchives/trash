#include <iostream>
#include "print.hpp"
#include "print.hpp"

int main(void) {
    printI(1, 2);
    printF(1.2, 3.9);
#ifndef NFUN_OVERLOAD
    print(2, 9);
    print(1.2, 3.2);
#endif
    return 0;
}
