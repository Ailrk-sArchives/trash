import ctypes
from typing import List, cast
from array import array
import os

"""
c_int is mutable as opposed to int in python.
"""

_file = 'libsample.so'
_path = os.path.join(
    *(os.path.split(os.path.abspath(__file__))[:-1] + (_file,)))
_mod = ctypes.cdll.LoadLibrary(_path)

# int gcd(int x, int y)
gcd = _mod.gcd
gcd.argtypes = (ctypes.c_int, ctypes.c_int)
gcd.restype = ctypes.c_int

# bool in_mandel(double x0, double y0, int n)
in_mandel = _mod.in_mandel
in_mandel.argtypes = (ctypes.c_double, ctypes.c_double, ctypes.c_int)
in_mandel.restype = ctypes.c_bool

# int divide(int a, int b, int *remainder) {
_divide = _mod.divide
_divide.argtypes = (ctypes.c_int, ctypes.c_int,
                    ctypes.POINTER(ctypes.c_int))
_divide.restype = ctypes.c_int


def divide(x, y):
    """
    wrapper around c calling convention that is
    not compatibile with python.
    """
    rem = ctypes.c_int()
    quot = _divide(x, y, rem)
    return quot, rem.value


# double avg(double *a, int n)
# special type for double*
class DoubleArrayType:
    """
    Map equivalent python sequence to c array.
    """

    def from_param(self, param):
        typename = type(param).__name__
        if hasattr(self, 'from_' + typename):
            return getattr(self, 'from_' + typename)(param)
        elif isinstance(param, ctypes.Array):
            return param
        else:
            raise TypeError(f"Can't convert {typename}")

    def from_array(self, param: array):
        if param.typecode != 'd':
            raise TypeError("Must be an array of doubles")
        ptr, _ = param.buffer_info()
        return ctypes.cast(
            ptr, ctypes.POINTER(ctypes.c_double))  # type: ignore

    def from_list(self, param: List[float]):
        val = ((ctypes.c_double) * len(param))(*param)
        return val

    from_tuple = from_list

    def from_ndarray(self, param):
        return param.ctypes.data_as(ctypes.POINTER(ctypes.c_double))


DoubleArray = DoubleArrayType()
_avg = _mod.avg
_avg.argtypes = (DoubleArray, ctypes.c_int)  # type: ignore
_avg.restype = ctypes.c_double


def avg(values):
    return _avg(values, len(values))


class Point(ctypes.Structure):
    _fields_ = [('x', ctypes.c_double),
                ('y', ctypes.c_double)]


distance = _mod.distance
distance.argtypes = (ctypes.POINTER(Point), ctypes.POINTER(Point))
distance.restype = ctypes.c_double
