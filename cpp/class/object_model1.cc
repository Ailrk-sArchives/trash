#include <cmath>
#include <iostream>

struct complex {
  float real;
  float img;
  float abs() const { return std::hypot(real, img); }
};

// is

struct complex_compiled {
  float real;
  float img;
};
float abs(complex *self) { return std::hypot(self->real, self->img); }
