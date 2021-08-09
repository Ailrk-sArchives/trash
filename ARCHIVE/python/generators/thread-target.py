from queue import Queue
from threading import Thread
import xml
from typing import List, Dict, Coroutine


def coroutine(func):
    def start(*args, **kwarfs):
        cr = func(*args, **kwarfs)
        next(cr)
        return cr
    return start


@coroutine
def buese_to_dicts(target: Coroutine):
    while True:
        event, value = (yield)
        if event == 'start' and value[0] == 'bus':
            busdict: Dict = {}
            fragments: List = []

            while True:
                event, value = (yield)
                if event == 'start':
                    fragments = []
                elif event == 'text':
                    fragments.append(value)
                elif event == 'end':
                    if value != 'bus':
                        busdict[value] = "".join(fragments)
                    else:
                        target.send(busdict)
                        break


@coroutine
def threaded(target: Coroutine):
    messages: Queue = Queue()

    def run_target():
        while True:
            item = messages.get()
            if item is GeneratorExit:
                target.close()
                return
            else:
                target.send(item)
    Thread(target=run_target).start()

    try:
        while True:
            item = (yield)
            messages.put(item)
    except GeneratorExit:
        messages.put(GeneratorExit)




