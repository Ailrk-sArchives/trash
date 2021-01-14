#include <iostream>
#include "test.h"


// Function overload only in CPP
void print(int a, int b) {
    std::cout << a  << "+" << b  << "=" << sumI(a, b) << std::endl;
}


void print(double a, double b) {
    std::cout << a  << "+" << b << "=" << sumF(a, b) << std::endl;
}


// add the extern "C" derivitive to declare function in C standard.
// No function overloading in C.
extern "C" void printI(int a, int b) {
    std::cout << a  << "+" << b << "=" << sumI(a, b) << std::endl;
}


extern "C" void printF(double a, double b) {
    std::cout << a  << "+" << b << "=" << sumF(a, b) << std::endl;
}
