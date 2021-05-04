#include <iostream>

// again, the rust example.
void compute1(const int &input, int &output) {
  if (input > 10) {
    output = 1;
  }
  if (input > 5) {
    output *= 2;
  }
}

// rewrite like this
void compute2(const int &input, int &output) {
  if (input > 10) {
    output = 1;
  } else if (input > 5) {
    output *= 2;
  }
}

int main() {

  int value = 10;
  int result = 0;

  // ok, this two have results.
  compute1(value, result);
  compute2(value, result);

  // have different results.
  // because input and output are aliases of the same value,
  // and compute1 mutate the value, which it's value is used for
  // branching.
  compute1(value, value);
  compute2(value, value);
}
