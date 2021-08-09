import random
from tombola import Tombola
from typing import Iterable, Generic, List, TypeVar


T = TypeVar('T')


class BingoCage(Tombola, Generic[T]):
    """ stack like """
    def __init__(self, items: Iterable):
        self._randomizer = random.SystemRandom()
        self._items: List[T] = []
        self.load(items)

    def load(self, items):
        self._items.extends(items)
        self._randomizer.shuffle(self._items)

    def pick(self):
        try:
            return self._items.pop()
        except IndexError:
            raise LookupError('pick from empty BingoCage')

    def __call__(self):
        self.pick()


class LotteryBlower(Tombola, Generic[T]):
    def __init__(self, iterable: Iterable[T]):
        self._balls = list(iterable)

    def load(self, iterable: Iterable[T]):
        self._balls.extend(iterable)

    def pick(self):
        try:
            position = random.randrange(len(self._balls))
        except ValueError:
            raise LookupError('pick from empty BingoCage')
        return self._balls.pop(position)

    def loaded(self):
        return bool(self._balls)

    def inspect(self):
        return tuple(sorted(self._balls))


@Tombola.register
class TomboList(List):
    """
    a virtual subclass, avoid multiinheritence
    isinstance will treat it as Tombola, but Tombola will
    not be in __mro__
    """

    def pick(self):
        if self:
            position = random.randrange(len(self))
            return self.pop(position)
        else:
            raise LookupError('pop from empty TomboList')

    load = list.extend

    def loaded(self):
        return bool(self)

    def inspect(self):
        return tuple(sorted(self))
