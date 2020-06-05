"""
Producer / Consumer

The buffer must have exlusive access when item is being added.
If a consumer arrives while buffer is empty, it blocks until a new item appear.

Be extremely careful when you have nested locks.
In particular,
When for a thread the release of a lock depends on another thread can
pass another lock, and the release of another lock is depends on the first
thread got unlock.
"""
from threading import Semaphore, Thread
from collections import deque
from random import randint


class BufferEnd(Exception):
    pass


class Buffer:
    def __init__(self):
        self._deque = deque()
        self._mutex = Semaphore(1)
        self._mutex_empty = Semaphore(0)

    def get(self):
        # NOTE: risk of DEAD LOCK
        # the senario that _mutex_empty is inside self._mutex
        # if consumer come first, lock _mutex and block at _mutex_empty,
        # then a producer come, it will block at _mutex and no one can proceed.
        self._mutex_empty.acquire()
        with self._mutex:
            print(self._deque)
            res = self._deque.popleft()
        return res

    def add(self, evt):
        with self._mutex:
            self._deque.append(evt)
        self._mutex_empty.release()


"""
Using BufferEnd Sentinel to send a end message.
The problem here is when there is aribitrary amont of
producers and consumers, how to shut consumers exactly when
all producers shutdown.
"""


def producer(buf: Buffer):
    tid = randint(0, 1000)
    with open('/dev/random', 'rb') as f:
        for i in range(5):
            raw = f.read(10)
            print(f"[producer] {tid} iter", i, ' raw ', raw)
            buf.add(hash(raw) % 10000)
    buf.add(BufferEnd)


def consumer(buf: Buffer):
    tid = randint(0, 1000)
    while True:
        print(f"[consumer] {tid} consuming ...")
        data = buf.get()
        if data is BufferEnd:
            break
        print(data)


if __name__ == "__main__":
    buf = Buffer()
    p1 = Thread(target=producer, args=(buf,))
    p2 = Thread(target=producer, args=(buf,))
    c1 = Thread(target=consumer, args=(buf,))
    c2 = Thread(target=consumer, args=(buf,))
    c3 = Thread(target=consumer, args=(buf,))

    p1.start()
    p2.start()
    c1.start()
    c2.start()
    c3.start()

    p1.join()
    p2.join()
    c1.join()
    c2.join()
    c3.join()
