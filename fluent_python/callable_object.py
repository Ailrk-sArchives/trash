import random


class BingoCage:
    def __init__(self, items):
        """constructor"""
        self._items = list(items)
        random.shuffle(self._items)

    def _pick(self):
        """pick"""
        try:
            return self._items.pop()
        except IndexError:
            raise LookupError('pick from empty BingoCage')

    def __call__(self):
        """callable"""
        return self._pick()


if __name__ == "__main__":
    bc = BingoCage((random.randint(0, 20) for _ in range(10)))
    print(bc.__call__.__doc__)
    print(bc())
    print(bc())
    print(bc())
    print(bc())
