import sys


class SkipBlockException(Exception):
    pass


class MyContext:
    def __init__(self, mode=0):
        self.mode = mode

    def __enter__(self):
        if self.mode == 1:
            print('block it...')

            sys.settrace(lambda *args, **kwargs: None)
            frame = sys._getframe(1)
            frame.f_trace = self.trace

    def trace(self, frame, event, arg):
        raise SkipBlockException()

    def __exit__(self, type, value, traceback):
        if type is None:
            return
        if issubclass(type, SkipBlockException):
            return True


if __name__ == "__main__":
    with MyContext(mode=0):
        print('1')

    with MyContext(mode=1):
        print('2')
