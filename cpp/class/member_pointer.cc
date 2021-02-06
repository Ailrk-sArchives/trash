#include <iostream>

class Car {
public:
  int speed;

  void func() {}
};

// pointer to member functions
void apply(Car *c, void (Car ::*func)()) {
  // again, here the operator is ->*
  (c->*func)();
}

int main(void) {
  // declare a pointer to member.
  // notice, Car::* is a type!
  int Car::*pSpeed = &Car::speed;

  // you can fake a member pointer up.
  // there is no double in Car, but it can stop you from declaring
  // a member pointer to double.
  // This is the feature being used to test if a type is a class.
  double Car::*p;

  Car c1;

  // what it realy does is a binop takes an objecet and an object
  // pointer.
  int a = c1.*pSpeed;
  int b = (&c1)->*pSpeed;

  c1.speed = 10;
  std::cout << "speed is: " << c1.speed << std::endl;

  c1.*pSpeed = 2;
  std::cout << "speed is: " << c1.speed << std::endl;

  return 0;
}
