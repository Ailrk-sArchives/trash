"""
Proxy `speak for` actual implementation
"""


class Implementation:
    def f(self):
        print("Implementation.f()")

    def g(self):
        print("Implementation.g()")

    def h(self):
        print("Implementation.h()")


class Proxy:
    def __init__(self):
        self.__implementation = Implementation()

    def f(self):
        self.__implementation.f()

    def g(self):
        self.__implementation.g()

    def h(self):
        self.__implementation.h()
