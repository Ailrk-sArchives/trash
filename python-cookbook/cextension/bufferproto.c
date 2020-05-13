#include <Python.h>

/* Generalized array type received by c extension
 * */

double avg(double *a, int n) {
  double sum;
  for (int i = 0; i < n; i++)
    sum += a[i];
  return sum / n;
}

static PyObject *py_avg(PyObject *self, PyObject *args) {
  PyObject *bufobj;
  Py_buffer view;

  double result;
  // "O" for python object
  if (!PyArg_ParseTuple(args, "O", &bufobj)) {
    return NULL;
  }

  // get passed py object into Py_buffer view
  // PyObject_GetBuffer  trying to obtain information of the underlying mem representation.
  if (PyObject_GetBuffer(bufobj, &view, PyBUF_ANY_CONTIGUOUS | PyBUF_FORMAT) ==
      -1) {
    return NULL;
  }

  if (view.ndim != 1) {
    PyErr_SetString(PyExc_TypeError, "Expected a 1-dimensinoal array");
    PyBuffer_Release(&view);
    return NULL;
  }

  if (strcmp(view.format, "d") != 0) {
    PyErr_SetString(PyExc_TypeError, "Expected an array of doubles");
    // for ref count
    PyBuffer_Release(&view);
    return NULL;
  }

  result = avg((double *)view.buf, view.shape[0]);

  PyBuffer_Release(&view);
  return Py_BuildValue("d", result);
}
