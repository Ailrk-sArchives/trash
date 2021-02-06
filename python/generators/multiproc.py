import pickle
import socket
from typing import Generator, List, Iterator
import queue
import threading


# Pickler/Unpickler
def gen_pickle(source):
    for item in source:
        yield pickle.dumps(item)


def get_unpickle(infile):
    while True:
        try:
            item = pickle.loads(infile)
            yield item
        except EOFError:
            return

# Sender/Receiver


def sendto(source, addr):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect(addr)
    for pitem in gen_pickle(source):
        s.sendall(pitem)
    s.close()


def receivefrom(addr):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    s.bind(addr)
    s.listen(5)

    c, a = s.accept()
    for item in get_unpickle(c.makefile()):
        yield item
    c.close()


# Consuming a queue
def consume_queue(thequeue: queue.Queue):
    while True:
        item = thequeue.get()
        if item is StopIteration:
            break
        yield item


# fanning out to multiple cosummer

# broadcasting
class Consumer:
    def send(self, item):
        print(self, "got", item)


def broadcast(source: Iterator, consumers: List[Consumer]):
    for item in source:
        for c in consumers:
            c.send(item)


# Network consumer
class NetConsumer:
    def __init__(self, addr):
        self.s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    def send(self, item):
        pitem = pickle.dumps(item)
        self.s.sendall(pitem)

    def close(self):
        self.s.close()


# Consumer Thread
class ConsumerThread(threading.Thread):
    def __init__(self, target):
        threading.Thread.__init__(self)
        self.setDaemon(True)
        self.in_queue = queue.Queue()
        self.target = target

    def send(self, item):
        self.in_queue.put(item)

    def generate(self):
        while True:
            item = self.in_queue.get()
            yield item

    def run(self):
        self.target(self.generate())
