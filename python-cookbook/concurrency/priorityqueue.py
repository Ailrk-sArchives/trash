"""
Implement your own thread safe data structure.
"""
import heapq
import threading

_sentinel = object()
sentinel_prori = 3


class PriorityQueue:
    def __init__(self):
        self._queue = []
        self._count = 0
        self._cv = threading.Condition()

    def put(self, item, priority):
        with self._cv:
            # note priority queue holds a tuple a priority and items.
            heapq.heappush(self._queue, (-priority, self._count, item))
            self._count += 1
            self._cv.notify()

    def get(self):
        with self._cv:
            while len(self._queue) == 0:
                # wati till new item is put.
                self._cv.wait()
            # pop item
            return heapq.heappop(self._queue)[-1]


def producer(pq: PriorityQueue):
    """ higher priority for even i"""
    for i in range(10):
        priority = 2 if i % 2 == 0 else 1
        pq.put(i, priority)
        print(f"[producer] data: {i}, priority: {priority}")
    pq.put(_sentinel, sentinel_prori)  # sentinel has higher priority.


def consumer(pq: PriorityQueue, tid: int):
    while True:
        data = pq.get()
        if data is _sentinel:
            pq.put(_sentinel, sentinel_prori)
            break
        print(f'[cosumer {tid}]', f'data: {data}')


if __name__ == "__main__":
    q = PriorityQueue()
    threads = [threading.Thread(target=consumer, args=(q, i))
               for i in range(10)]
    threads.append(threading.Thread(target=producer, args=(q,)))

    for t in threads:
        t.start()

    for t in threads:
        t.join()


