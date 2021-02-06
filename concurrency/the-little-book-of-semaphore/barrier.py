from threading import Semaphore, Thread
from random import randint
from time import sleep

thread_num = 5

"""
Generalize rendezous. Barrier force execution
join at a particular point.

format:
    rendezvous
    critical section
No thread get into critical sectin until all threads
passed rendezvous

Note avoid deadlock by muliple layers of mutexes.
"""


class Barrier:
    """ Join Barrier """

    def __init__(self, n: int):
        self._n = n
        self._n_arrived = 0

        self._mutex = Semaphore(1)    # one thread can pass.
        self._barrier = Semaphore(0)  # no thread can pass.

    def wait(self):
        # use mutex to protect the counter.
        self._mutex.acquire()
        self._n_arrived += 1
        if self._n_arrived == self._n:
            self._barrier.release()
        self._mutex.release()

        # when all thread passed, the last one make the first release
        # after that other threads will starts release their own acquire.

        self._turnstile()
        self._rewind()

    def _rewind(self):
        """ lock again after all threads passed. """
        self._mutex.acquire()
        self._n_arrived -= 1
        if self._n_arrived == 0:
            self._barrier.acquire()
        self._mutex.release()

    def _turnstile(self):
        """ each thread will lock in turnstil until a first release """
        # acquire and release in rapid succession.
        # lock at the very beginning.
        self._barrier.acquire()

        # release at the very end.
        self._barrier.release()


def worker(barrier: Barrier):
    print("rendezvous")
    sleep(randint(0, 1))
    barrier.wait()

    print("critical")


if __name__ == "__main__":

    barrier = Barrier(thread_num)

    for idx in range(100):
        print(idx, "============")
        threads = []
        for i in range(thread_num):
            t = Thread(target=worker, args=(barrier,))
            threads.append(t)
            t.start()

        for t in threads:
            t.join()
