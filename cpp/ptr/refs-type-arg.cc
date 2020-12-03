#include <iostream>

// pass by reference is more efficient then pass by value.
// The reason is very clear, instead of copying the
// argument, you essentially passing in an address.

struct Date {
  short month;
  short day;
  short year;
};

// a const ptr const int
const int *const p1 = nullptr;

// a const ref to int
// ref itself cannot chance, constness act on the value.
const int &p2 = 1;

// an example with rather long body.
// passing by const reference.
// we only need to see the data, and don't need to mutate it.
long get_date_of_year(const Date &date) {
  static int c_days_in_month[] = {31, 28, 31, 30, 31, 30,
                                  31, 31, 30, 31, 30, 31};
  long date_of_year = 0;

  for (int i = 0; i < date.month - 1; ++i) {
    date_of_year += c_days_in_month[i];
  }

  if (date.month > 2 &&
      ((date.year % 100 != 0 || date.year % 400 == 0) && date.year % 4 == 0)) {
    date_of_year++;
  }

  date_of_year *= 10000;
  date_of_year += date.year;
  return date_of_year;
}

// function can also be declared to return a reference type.
// Reasons to do so:
//  1. return value is large enough, ref is faster then copy
//  2. the type of the function must be a rval
//      e.g method overload, method must evaluate to a lvalue.
//  3. the referred to object will not go out of scope after return.

class Point {
public:
  // accessor functions as reference type.
  // You can return refrence because referred-to values are
  // not out of scope when the fucntion return,
  // they are still in the object.
  unsigned &x();
  unsigned &y();

  // declared at class scope
  unsigned obj_x;
  unsigned obj_y;
};

unsigned &Point::x() { return obj_x; }

unsigned &Point::y() { return obj_y; }

int main(void) {
  Date date{11, 25, 2020};
  std::cout << get_date_of_year(date) << std::endl;

  Point point;

  // returned a lval ref, you can assign to it.
  // Notice at this point value obj_x and obj_y are still
  // hold in the object point, and point is not out of scope
  // yet. That's why you can return a reference and be ok with it.
  point.x() = 10;
  point.y() = 20;

  std::cout << point.x() << " " << point.y() << std::endl;

  return 0;
}

// this is bad, a get destroyed after return,
// and you get the same problem as return a point to
// local variable.
volatile long &bad_return() {
  long a = 100;
  return a;
}
