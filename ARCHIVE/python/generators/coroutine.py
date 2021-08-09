
def consumer(func):
    def start(*args, **kwargs):
        c = func(*args, **kwargs)
        next(c)
        return c
    return start


@consumer
def recv_count():
    try:
        while True:
            n = (yield)
            print("T-minux", n)
    except GeneratorExit:
        print("Kaboom!")



