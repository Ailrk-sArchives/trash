"""
Protocals
"""
from typing import NamedTuple
from collections.abc import Sequence
from random import shuffle


class Foo:
    """
    Python can infer entire interface from partialy implemented class.
    This class only have __getitem__, but most protocols from
    Sequence can be used directly without being defined.
    """

    def __getitem__(self, pos):
        return range(0, 30, 10)[pos]

    def foo(self):
        """ iterable even without __iter__ """
        for i in self:
            print(i)

    def foo1(self, x):
        """ act as a container even without __contains__ """
        return x in self


class Card(NamedTuple):
    rank: str
    suit: str


class FrenchDeck(Sequence):
    """
    It is basically a Sequence by implementing __len__ and
    __getitem__
    """
    ranks = [str(n) for n in range(2, 11)] + list('JQKA')
    suits = 'spades diamonds clubs hearts'.split()

    def __init__(self):
        self._cards = [Card(rank, suit)
                       for suit in self.suits
                       for rank in self.ranks]

    def __len__(self):
        return len(self._cards)

    def __getitem__(self, position):
        return self._cards[position]

    def __delitem__(self, position):
        del self._cards[position]

    def shuffle(self):
        """
        deckMonkey patch a necessary function __setitem__
        in runtime to allow shuffle accept it.
        """
        def _set(self: 'FrenchDeck', position, card):
            self._cards[position] = card
        self.__setitem__ = _set
        shuffle(self)


