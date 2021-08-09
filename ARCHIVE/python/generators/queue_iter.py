from queue import Queue

q: Queue = Queue()

for i in range(100):
    q.put(i)


def it():
    while not q.empty():
        yield q.get()

