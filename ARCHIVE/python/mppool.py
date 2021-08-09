from multiprocessing import get_context, Pool
from itertools import tee
from random import random
import time
import os


"""
Multiprocessing use pickle to send objects between processes.
local function is not pickle thus cannot be used for multiprocessing.
all functions needs to be at the top level.

The process spawning mode is very important because they can change the
behavior of the program drastically.

In unix fork is the default spawn methods, which will fork the entire mem of
current process. (note, threads is not copied, so two processes will have
same set of threads.)

Another type of spawning is 'spawn', which will create a new, "empty" python
interpreter, then start the process from the scratch.

One common problem is fork while there is threads running already.
If there is lock in locked state, the lock will be copied by the process,
but since threads are not copied, there is this propability that a
thread that shouldn't block got blocked.
"""


def worker(id: int):
    time.sleep(random() * 2)
    print(id)


def main():
    iterator = range(10)
    (iterator1, iterator2, iterator3, iterator4) = tee(iterator, 4)
    with Pool(7) as pool:
        """
        imap and imap_unordered are lazy version of map, by default they
        will slice one element from the iterator for one worker.
        chunk_size param can change the work load per worker.
        """
        print('----- imap_unordered')
        for n in pool.imap_unordered(worker, iterator1):
            pass

        print('----- imap')
        for n in pool.imap(worker, iterator2):
            pass

        """
        map and map_async are egar evaluations, the program will block until
        all results are returned.

        Before separate the iterator map will convert it into a list, which
        could be problemati if the iterator is very big.

        map will return the result right away, whihle map_async return a
        AsyncResult, which works like future, and one can get the value by
        call AsyncResult.get()
        """
        print('----- map')
        for n in pool.map(worker, iterator3):
            pass

        print('----- map_async')
        res = pool.map_async(worker, iterator4)
        for n in res.get():
            pass


if __name__ == "__main__":
    main()

