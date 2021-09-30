#include <iostream>

class A {
    using T1 = int;

  public:
    using TP = char;
};

class B : public A {};

int main() {
    A::TP a = 10;
    B::TP b = 88;
}
