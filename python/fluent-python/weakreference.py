import weakref
from typing import MutableMapping

"""
Weakreference to an object does not increase its reference count.
It doesn't prevent an object from being gabage collected.
weakref are useful in caching.
"""


class Cheese:

    def __init__(self, kind):
        self.kind = kind

    def __repr__(self):
        return 'Cheese(%r)' % self.kind


if __name__ == "__main__":
    stock: MutableMapping = weakref.WeakValueDictionary()
    catalog = [Cheese('Red Leicester'), Cheese('Tilsit'),
               Cheese('Brie'), Cheese('Parmesan')]
    for cheese in catalog:
        stock[cheese.kind] = cheese

    print(sorted(stock.keys()))
    del catalog
    print(sorted(stock.keys()))
    del cheese  # cheese as temporary var is still alive.
    print(sorted(stock.keys()))

