"""
publish/subscribe messaging.
Use a separate exchange obj to route messages.
"""

from collections import defaultdict
from typing import Dict, Set, Tuple, Sequence
from abc import ABC, abstractmethod
from contextlib import contextmanager
import multiprocessing
from functools import partial
import random

Matrix = Sequence[Sequence]


class Task(ABC):
    @abstractmethod
    def send(self, msg):
        """ send message"""


class Exchange:
    """
    Benefit:
        1. simplify wring.
        2. can broadcast msg to a set of tasks
        3. works for any task like object with send. task, generator etc..
    """

    def __init__(self):
        self._subscriber: Set[Task] = set()

    def attach(self, task):
        self._subscriber.add(task)

    def detach(self, task):
        self._subscriber.remove(task)

    @contextmanager
    def subscribe(self, *tasks):
        for task in tasks:
            self.attach(task)
        try:
            yield
        finally:
            for task in tasks:
                self.detach(task)

    def send(self, msg):
        for subscriber in self._subscriber:
            subscriber.send(msg)


_exchange: Dict[str, Exchange] = defaultdict(Exchange)


def get_exchange(name):
    return _exchange[name]


class DisplayMessageTask(Task):
    def __init__(self):
        self.count = 0

    def send(self, msg: str):
        self.count += 1
        print('msg[{}]: {!r}'.format(self.count, msg))


class MatrixMultiplyTask(Task):
    def __init__(self):
        ...

    def send(self, msg: Tuple[Matrix, Matrix]) -> Matrix:
        m1, m2 = msg
        return self.mul(m1, m2)

    def mul(self, m1: Matrix, m2: Matrix) -> Matrix:
        if len(m1[0]) != len(m2):
            raise TypeError("invalid matrix shape")

        with multiprocessing.Pool(4) as pool:
            res = [pool.map(partial(self.dot, v1), zip(*m2)) for v1 in m1]
            __import__('pprint').pprint(res)
        return res

    @staticmethod
    def dot(v1: Tuple, v2: Tuple) -> float:
        return sum(map(lambda vtup: vtup[0] * vtup[1], zip(v1, v2)))


if __name__ == "__main__":
    exc = get_exchange('name')
    matexc = get_exchange('matrix')

    task1 = DisplayMessageTask()
    task2 = DisplayMessageTask()
    with exc.subscribe(task1, task2):
        exc.send("msg1")
        exc.send("msg2")

    task3 = MatrixMultiplyTask()
    with matexc.subscribe(task3):
        m1 = [[random.randint(0, 10) for _ in range(15)]
              for _ in range(15)]
        __import__('pprint').pprint(m1)
        m2 = [[(15 if i == j else 0) for j in range(15)]
              for i in range(15)]
        __import__('pprint').pprint(m2)
        matexc.send((m1, m2))
