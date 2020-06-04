from threading import Semaphore, Thread
from random import randint
from time import sleep

thread_num = 10

"""
Generalize rendezous. Barrier force execution
join at a particular point.

format:
    rendezvous
    critical section
No thread get into critical sectin until all threads
passed rendezvous
"""


class Barrier:
    """ Join Barrier """

    def __init__(self, n: int):
        self._n = n
        self._arrived_n = 0
        self._mutex = Semaphore(1)
        self._barrier = Semaphore(0)
        for _ in range(self._n):
            self._barrier.acquire()

    def wait(self):
        """
        use mutex to protect the increment.
        each worker release once.
        """
        self._mutex.acquire()
        self._arrived_n += 1
        self._barrier.release()
        self._mutex.release()


def worker(barrier: Barrier):
    print("rendezvous")
    sleep(randint(0, 3))
    barrier.wait()

    print("critical")


if __name__ == "__main__":

    barrier = Barrier(thread_num)

    threads = []
    for i in range(thread_num):
        t = Thread(target=worker, args=(barrier,))
        threads.append(t)
        t.start()

    for t in threads:
        t.join()

