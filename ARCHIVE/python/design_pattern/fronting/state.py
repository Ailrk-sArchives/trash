"""
Adds more implementation for proxy.
"""


class State:
    def __init__(self, imp):
        self.__implementation = imp

    def changeImp(self, newImp):
        self.__implementation = newImp

    def __getattr__(self, name):
        return getattr(self.__implementation, name)


class Implementation1:
    def f(self):
        print("Implementation.f()")

    def g(self):
        print("Implementation.g()")


class Implementation2:
    def f(self):
        print("Implementation.f()")

    def g(self):
        print("Implementation.g()")


