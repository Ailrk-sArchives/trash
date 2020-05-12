"""
Coordinate shutdown of producer and consumer with queue.
"""
from queue import Queue
from threading import Thread


# a signal object be pushed into queue to
# notify consumers to shutdown.
_sentinel = object()


def producer(out_q: Queue):
    """ when job finished put sentinel into queue """
    for i in range(100, 120):
        out_q.put(i)
    out_q.put(_sentinel)


def consumer(in_q: Queue, tid: int):
    while True:
        data = in_q.get()

        if data is _sentinel:
            # put sentinel back to queue to inform other consumer to shutdown
            in_q.put(_sentinel)
            break

        print(f'cosumer {tid}', f'data: {data}')


if __name__ == "__main__":
    queue: Queue = Queue(maxsize=10)
    threads = [Thread(target=consumer, args=(queue, i)) for i in range(10)]
    threads.append(Thread(target=producer, args=(queue,)))

    for t in threads:
        t.start()

    for t in threads:
        t.join()


