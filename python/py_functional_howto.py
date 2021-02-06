import itertools
import functools
from typing import Iterable, Generator, Callable, List
import random

import pysnooper


def printname(f):
    @functools.wraps(f)
    def wrapper(*args, **kwargs):
        print(f.__name__ + ': ')
        f(*args, **kwargs)
        print()
    return wrapper


# interator
@pysnooper.snoop()
@printname
def top1_iterator():
    li: List = [1, 2, 3]
    it: Iterable = iter(li)
    for i in range(len(li)):
        print(next(it))


@printname
def top2_iterator():
    li: List = [3, 4, 5]
    for i in iter(li):
        print(i)


@printname
def top3_generator():
    line_list: List = ['  line 1\n', ' line 2 \n']
    stripped_iter: Iterable = (line.strip() for line in line_list)
    stripped_list: List = [line.strip() for line in line_list]

    # lazyeval
    _ = list(map(print, stripped_iter))
    _ = list(map(print, stripped_list))


@printname
def top4_generator():

    def gen_ints(n: int) -> Generator:
        i: int = 1
        while i <= n:
            yield i
            i += 1

    for i in gen_ints(3):
        print(i)


@printname
def top5_define_iterator():
    class Iter:

        def __iter__(self):
            self.num = 0
            return self

        def __next__(self):
            self.num += 1
            if self.num > 3:
                raise StopIteration(3)

            return self.num

    for i in Iter():
        print(i)


@printname
def top6_inorder_tree():
    def inorder(t):
        if t:
            for x in inorder(t.left):
                yield x
            yield t.label
            for x in inorder(t.right):
                yield x


@printname
def top7_yield():
    def counter(maximum: int) -> Generator:
        i: int = 0
        while i < maximum:
            val = (yield i)
            if val is not None:
                i = val
            else:
                i += 1
    print(list(counter(4)))

    it: Generator = counter(10)
    next(it)
    it.send(7)  # skip steps
    print(list(it))


@printname
def top8_map():
    print(list(map(lambda s: s.upper(), ['two', 'words'])))
    print(list(filter(lambda x: x % 2 == 0, range(0, 10))))
    print(list((x for x in range(0, 10) if x % 2 != 0)))


@printname
def top9_enum():
    def get_ints(maximum: int) -> Generator:
        i = 1000
        while i < 1000 + maximum:
            val = (yield i)
            if val is not None:
                i = val
            else:
                i += 1
    for i, v in enumerate(get_ints(3)):
        print(i, v)


@printname
def top10_anyall():
    print(all(filter(lambda x: x > 3000, random.sample(range(6000), 1000))))
    print(any(filter(lambda x: x > 5999, random.sample(range(6000), 1000))))


@printname
def top11_zip():
    print(dict(zip(['good', 'bad'], [7, 13])))


@printname
def top12_itertool_funcs():
    from itertools import (
        islice, count, cycle, repeat, chain, tee, starmap, combinations,
        permutations, groupby)

    print(list(islice(count(), 100, 200, 5)))
    print(tuple(islice(cycle([1, 2, 3]), 300, 400, 5)))
    print(tuple(repeat('cat', 10)))
    print(tuple(chain(['a', 'b'], [1, 2], (x for x in range(0, 10) if x > 4))))

    # tee a generator
    iter1, iter2 = tee((x + 1 for x in range(0, 10) if x % 2 == 0))
    print(list(map(lambda x: x * 100, iter1)))
    print(list(iter2))

    # itermap
    from operator import add
    from os.path import join
    print(list(starmap(join, [('/bin', 'haskell'), ('/usr', 'c++')])))
    print(list(starmap(add, [(2, 3), (4, 8)])))

    print(list(combinations(range(4), 2)))
    print(list(permutations(range(4), 2)))

    # Group by
    city: List = [('Beijing', 'Jin'),
                  ('Nantong', 'Su'),
                  ('Suzhou', 'Su'),
                  ('Shanghai', 'Hu')]

    def get_province(city):
        _, prov = city
        return prov
    Jin, Su, Hu = groupby(city, get_province)


def top13_functools():
    from functools import partial, reduce

    def log(msg, subsys):
        print(f'{msg}: {subsys}')

    serverlog = partial(log, subsys='server')
    serverlog('Unable to open socket')

    from operator import mul
    print(reduce(mul, (x for x in range(1, 100) if x % 2 == 0), 1))


if __name__ == "__main__":
    top1_iterator()
    top2_iterator()
    top3_generator()
    top4_generator()
    top5_define_iterator()
    top6_inorder_tree()
    top7_yield()
    top8_map()
    top9_enum()
    top10_anyall()
    top11_zip()
    top12_itertool_funcs()
    top13_functools()
