from threading import Semaphore, Thread
from time import sleep

"""
Rendezvous. Two threads rendezvous at a point of
execution, neither is allowed to proceed until both has
arrived.
a1 | b1 < a2 | b2

termys:
    V, inc, signal, release
    P, dec, wait, acquire
"""


def worker1(sema: Semaphore, semb: Semaphore):
    for i in range(30000000):
        ...
    print('[a] a1', sema._value, semb._value)

    sema.release()  # +
    semb.acquire()  # -

    for i in range(30000000):
        ...
    print('[a] a2', sema._value, semb._value)


def worker2(sema: Semaphore, semb: Semaphore):
    for i in range(30000):
        ...
    print('[b] b1', sema._value, semb._value)

    semb.release()  # +
    sema.acquire()  # -

    print('[b] b2', sema._value, semb._value)


if __name__ == "__main__":
    sema = Semaphore(0)
    semb = Semaphore(0)

    t1 = Thread(target=worker1, args=(sema, semb))
    t2 = Thread(target=worker2, args=(sema, semb))
    t1.start()
    t2.start()

    t1.join()

