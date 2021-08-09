import types
import time
import random


# non-data descriptor: only __get__() is implemented
# data descriptor: has __set__() or __delete__()

class VerboseAttribute():
    """
    A property object that implemented the descriptor protocol
    """

    def __get__(self, obj, type=None) -> object:
        print("accessing attribute to get the value")
        return 99

    def __set__(self, obj, value) -> None:
        print("accessing attribute to set the value")
        raise AttributeError("Can not change the value")


class Foo1:
    attribute = VerboseAttribute()


# with property
class Foo2():
    @property
    def attribute(self) -> object:
        print("accessing attribute to get the value")
        return 99

    @attribute.setter
    def attribute(self, value) -> None:
        print("accessing attribute to get the value")
        raise AttributeError("Cannot change the value")


class Foo3:

    def getter(self) -> object:
        print("accessing attribute to get the value")
        return 99

    def getter(self) -> object:
        print("good")
        return 99

    attribute = property(getter, setter)


# How does python transalte obj.method(*args) into method(obj, *args)
class Function:
    # ...
    # when the function is access with dot notation, __get__ is called
    # and bound method is returned

    def __get__(self, obj, objtype=None):
        "simulate func_descr_get()"
        if obj is None:
            return self
        return types.MethodType(self, obj)


class StaticMehod:
    """
    StaticMehod simply ignore the obj
    call f with regular args
    """

    def __init__(self, f):
        self.f = f

    def __get__(self, obj, objtype=None):
        return self.f


class ClassMethod:
    """
    PyyClassMethod_Type()
    call f with class as the first argument
    """

    def __init__(self, f):
        self.f = f

    def __get__(self, obj, klass=None):
        if klass is None:
            klass = type(obj)

        def newfunc(*args):
            return self.f(klass, *args)
        return newfunc

# look up chain
# 1. data descriptor
# 2. __dict__
# 3. non-data descriptor
# 4. type(self).__dict__
# 5. type(self).__base__.__dict__

# descriptor


class OneDigitNumericValue:
    """
    descriptor. data stored in owner object.
    automatically get assigned a name as identifier.
    """

    def __set_name__(self, owner, name):
        self.name = name

    def __get__(self, obj, type=None) -> object:
        return obj.__dict__.get(self.name) or 0

    def __set__(self, obj, value) -> None:
        obj.__dict__[self.name] = value


class Foo4:
    number = OneDigitNumericValue()


# Lazy properties. result get cached after first invoke.
class LazyProperty:
    """
    LazyProperty is a non data descriptor.
    first time get called __get__ will executes f automatically.
    the result is stored in __dict__.
    __dict__ has higher priority than non data discriptor
    so next time when meaning_of_life is accessed
    the value will be get from __dict__ directly.
    """

    def __init__(self, f):
        self.f = f
        self.name = f.__name__

    def __get__(self, obj, type=None) -> object:
        obj.__dict__.self.name = self.f(obj)
        return obj.__dict__[self.name]


class DeepThough:
    @LazyProperty
    def meaning_of_life(self):
        time.sleep(3)
        return 99


if __name__ == "__main__":
    pass
