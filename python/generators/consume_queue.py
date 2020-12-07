import queue, threading


def consume_queue(thequeue: queue.Queue):
    while True:
        item = thequeue.get()
        if item is StopIteration:
            break
        yield item


def consumer(q):
    for item in consume_queue(q):
        print('Consumed', item)
    print('Done')


if __name__ == "__main__":
    inq: queue.Queue = queue.Queue()

    con_thread = threading.Thread(target=consumer, args=(inq,))
    con_thread.start()

    for i in range(100):
        inq.put(i)
    inq.put(StopIteration)

