#include <gsl/gsl_math.h>
#include <gsl/gsl_sf_bessel.h>
#include <stdio.h>

// Topic: link with external libraries
// playing with shared libaries, static libaries,
// and some other linking techniques.

// gcc gsl.c -o a -lgsl -lgslcblas -lm
// or you can use -lcblas for the BLAS library.
// or add atlas support for fast matrix operation

// If you want to link to a shared libary, you need to be
// sure the .so file is in the path
//   $ LD_LIBRARY_PATH
// to set the load library path, do this in the shell:
//   $ LD_LIBRARY_PATH = /usr/local/lib
//   $ export LD_LIBRARY_PATH

// gsl allows you to swap out the native standard library and use the
// gsl's version
// It's normally better to use the native version because they usually
// have better platform specific optimization.
#ifndef HAVE_HYPOT
#define hypot gsl_hypot
double hypot_sum(double a, double b) {
  return hypot(a, b) + hypot(a + 1, b + 1);   // here we are using gsl function.
}
#endif

// alternative optimized functions
// again just macro substitute different versions

int main(void) {
  int e = 0;
  double f = gsl_frexp(12, &e);
  printf("the result: %f, %d", f, e);

  double x = 5.0;
  double y = gsl_sf_bessel_J0(x);
  printf("J1%g = %.18e\n", x, y);

  return 0;
}
