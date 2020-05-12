"""
Actor model. simple approach to concurrency and distributed computing.
"""
from queue import Queue
from threading import Thread, Event
from abc import ABC, abstractmethod
from typing import Generic, TypeVar


T = TypeVar('T')


class ActorExit(Exception):
    pass


class Actor(ABC, Generic[T]):
    """
    communication with actors is one way and async.
    """
    def __init__(self):
        self._queue: Queue[T] = Queue()

    def send(self, msg: T):
        """
        send message to the actor
        """
        self._queue.put(msg)

    def recv(self):
        """
        receive an message.
        """
        msg = self._queue.get()
        if msg is ActorExit:
            raise ActorExit()
        return msg

    def close(self):
        self.send(ActorExit)

    def start(self):
        """
        start concurrent execution
        """
        self._terminated = Event()
        t = Thread(target=self._bootstrap)
        t.daemon = True
        t.start()

    def _bootstrap(self):
        try:
            self.run()
        except ActorExit:
            pass
        finally:
            self._terminated.set()

    def join(self):
        self._terminated.wait()

    @abstractmethod
    def run(self):
        """ override """
        raise NotImplementedError("Actor need to be override")


class PrintActor(Actor):
    def run(self):
        while True:
            msg = self.recv()
            print("Got: ", msg)


class TaggedActor(Actor):
    """
    allows call different methods according to tag message received.
    """
    def run(self):
        while True:
            tag, *payload = self.recv()
            getattr(self, 'do_' + tag)(*payload)

    def do_A(self, x):
        print('running A: ', x)

    def do_B(self, *args):
        print('running A: ', *args)


class Result:
    """ Promiseish """
    def __init__(self):
        self._evt = Event()
        self._result = None

    def set_result(self, value):
        self._result = value
        self._evt.set()

    def result(self):
        self._evt.wait()
        return self._result


class WorkerActor(Actor):
    def submit(self, func, *args, **kwargs):
        r = Result()
        self.send((func, args, kwargs, r))
        return r

    def run(self):
        while True:
            func, args, kwargs, r = self.recv()
            r.set_result(func(*args, **kwargs))


if __name__ == "__main__":
    worker1 = WorkerActor()
    worker2 = WorkerActor()
    worker1.start()
    worker2.start()

    for i in range(0, 50):
        r = worker1.submit(pow, 2, i)
        print("worker 1", r.result())
    for i in range(0, 50):
        r = worker2.submit(pow, i, 2)
        print("worker 2", r.result())

    worker1.close()
    worker1.join()
    worker2.close()
    worker2.join()
