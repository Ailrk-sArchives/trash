from threading import Semaphore, Thread
from typing import Callable
from random import randint


class Queue:
    """
    Initial value for Semaphore is 0, and it cannot
    be released unless there is a thread waiting.
    """

    def __init__(self, fn: Callable):
        self._token = Semaphore(0)
        self._fn = fn

    @property
    def token(self):
        return self._token

    def assign_lead(self, other: 'Queue'):
        self._other = other
        self._other._other = self

    def wait_another(self):
        self._other.token.release()
        self._token.acquire()

    def start(self):
        self.t = Thread(target=self.run)
        self.t.start()

    def run(self):
        """ override """
        self.wait_another()
        self._fn()


def lead():
    print("leading")


def follow():
    print("following")


if __name__ == "__main__":
    leader = Queue(lead)
    follower = Queue(follow)
    follower.assign_lead(leader)

    leader.start()
    follower.start()

