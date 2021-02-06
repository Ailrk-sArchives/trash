"""
Interface for python
"""
from typing import TypeVar, Generic, Iterable, Tuple
from collections.abc import Sequence
from abc import abstractmethod, ABC

T = TypeVar('T')


class Tombola(ABC, Generic[T]):
    """
    non repeating random container.
    concrete method in abc should only depends on
    methods in the same abc.
    """

    @abstractmethod
    def load(self, iterable: Iterable[T]):
        """ Add items from an iterable """

    @abstractmethod
    def pick(self) -> T:
        """
        Pop a random item out.
        @raise e: LookupError
        """

    def loaded(self) -> bool:
        return bool(self.inspect())

    def inspect(self) -> Tuple[T, ...]:
        items = []
        while True:
            try:
                items.append(self.pick())
            except LookupError:
                break
        self.load(items)
        return tuple(sorted(items))

    @classmethod
    def __subclasshook__(cls, c):
        methods = ("__load__", "pick", "loaded", "inspect")
        if cls is Tombola:
            if any(method in b.__dict__
                    for b in c.__mro__
                    for method in methods):
                return True
        return NotImplemented
