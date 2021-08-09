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
import time

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
        if len(m1[0]) != len(m2):
            raise TypeError("invalid matrix shape")

        start = time.time()
        res1 = self.mul(m1, m2)
        print('time for parallel: ', time.time() - start)

        start = time.time()
        self.mul1(m1, m2)
        print('time for sequential: ', time.time() - start)

        return res1

    def mul(self, m1: Matrix, m2: Matrix) -> Matrix:
        res = []
        with multiprocessing.Pool(7) as pool:
            for v in m1:
                dotonv = partial(self.dot, v)
                vout = pool.map(dotonv, zip(*m2))
                res.append(vout)
        return res

    def mul1(self, m1: Matrix, m2: Matrix) -> Matrix:
        """ sequential """
        res = []
        for v in m1:
            dotonv = partial(self.dot, v)
            vout = map(dotonv, zip(*m2))
            res.append(list(vout))
        return res

    @staticmethod
    def dot(v1: Tuple, v2: Tuple) -> float:
        return sum(map(lambda vtup: vtup[0] * vtup[1], zip(v1, v2)))


def init_square_matrix(nrow) -> Tuple[Matrix, Matrix]:
    m1 = [[random.randint(0, 10) for _ in range(nrow)]
          for _ in range(nrow)]
    m2 = [[(15 if i == j else 0) for j in range(nrow)]
          for i in range(nrow)]
    print('finished initialization')

    return m1, m2


if __name__ == "__main__":
    exc = get_exchange('name')
    matexc = get_exchange('matrix')

#     task1 = DisplayMessageTask()
#     task2 = DisplayMessageTask()
#     with exc.subscribe(task1, task2):
#         exc.send("msg1")
#         exc.send("msg2")

    task3 = MatrixMultiplyTask()
    with matexc.subscribe(task3):
        matexc.send(init_square_matrix(400))

