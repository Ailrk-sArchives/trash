import time
import functools


def clock(fmt='[{elapsed:0.8f}s {name}({args}) -> {result}]'):
    def decorate(f):
        @functools.wraps(f)
        def clocked(*args, **kwargs):
            t0 = time.perf_counter()
            result = f(*args, **kwargs)
            elapsed = time.perf_counter() - t0
            name = f.__name__
            args = ', '.join(repr(arg) for arg in args)
            print(fmt.format(**locals()))
            return result
        return clocked
    return decorate


@clock('{elapsed}+{name}+{args}{result}')
def snooze(seconds):
    time.sleep(seconds)


@clock()
def factorial(n):
    return 1 if n < 2 else n * factorial(n - 1)


@clock()
def fib_vanilla(n):
    return n if n < 2 else fib_vanilla(n - 2) + fib_vanilla(n - 1)


@functools.lru_cache(maxsize=128, typed=False)
@clock()
def fib_cached(n):
    return n if n < 2 else fib_vanilla(n - 2) + fib_vanilla(n - 1)


if __name__ == "__main__":
    print('*' * 40, 'snooze(.123)')
    snooze(.123)
    print('*' * 40, 'factorial(5)')
    print('40! =', factorial(5))

    print('*' * 40, 'fib_vanilla(5)')
    print('fib_vanilla')
    fib_vanilla(5)

    print('*' * 40, 'fib_cached(5)')
    print('fib_cached')
    fib_cached(5)
