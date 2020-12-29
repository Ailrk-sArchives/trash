#include <Python.h>
#include "sample.h"

static PyObject *py_gcd(PyObject *self, PyObject *args) {
  int x, y, result;
  if (!PyArg_ParseTuple(args, "li", &x, &y)) {
    return NULL;
  }
  result = gcd(x, y);
  return Py_BuildValue("i", result);
}

static PyObject *py_in_mendel(PyObject *self, PyObject *args) {
  double x0, y0;
  int n;
  int result;

    if (!PyArg_ParseTuple(args, "ddi", &x0, &y0, &n)) {
        return NULL;
    }

    result = in_mandel(x0, y0, n);
    return Py_BuildValue("i", result);
}

static PyObject *py_divide(PyObject *self, PyObject *args) {
  int a, b, quotient, remainder;
  if (!PyArg_ParseTuple(args, "li", &a, &b)) {
    return NULL;
  }

  quotient = divide(a, b, &remainder);
  return Py_BuildValue("(ii)", quotient, remainder);
}

static PyMethodDef SampleMethods[] = {
  {"gcd", py_gcd, METH_VARARGS, "Greatest common divisor"},
  {"in_mandel", py_in_mendel, METH_VARARGS, "Manderbrot test"},
  {"divide", py_divide, METH_VARARGS, "Integer division"},
  {NULL, NULL, 0, NULL}
};

static struct PyModuleDef samplemodule = {
  PyModuleDef_HEAD_INIT,
  "sample",
  "A sample module",
  -1,
  SampleMethods
};

PyMODINIT_FUNC
PyInit_sample(void) {
  return PyModule_Create(&samplemodule);
}



