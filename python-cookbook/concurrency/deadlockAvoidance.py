import threading
from contextlib import contextmanager
import random

# thread local state to store information on lock already acquired.
_local = threading.local()


@contextmanager
def acquire(*locks):
    locks = sorted(locks, key=lambda x: id(x))
    acquired = getattr(_local, 'acquired', [])

    # make sure lock order of previously acquired locks is not violated
    if acquired and max(id(lock) for lock in acquired) >= id(locks[0]):
        raise RuntimeError("Lock Order Violation")

    acquired.extend(locks)
    _local.acquired = acquired

    try:
        for lock in locks:
            lock.acquire()
        yield
    finally:
        for lock in reversed(locks):
            lock.release()
        del acquired[:]


if __name__ == "__main__":
    # a quirk. You cannot use [Lock()] * 3 because it copy reference
    locks = [threading.Lock(),
             threading.Lock(),
             threading.Lock()]

    def worker(tid: int):
        while True:
            random.shuffle(locks)
            with acquire(*locks):
                print(f"Thread {tid}")

    threads = [threading.Thread(target=worker, args=(i,))
               for i in range(5000)]

    for t in threads:
        t.start()


