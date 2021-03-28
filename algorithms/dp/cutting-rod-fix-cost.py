import functools
import typing as t
rod_price = [-1, 1, 5, 8, 9, 10, 17, 17, 20, 24, 30]

# cutting rod with constant cost on each cut


def maximum(a, b):
    return a if a > b else b


def cache(f):
    cache_ = dict()

    @functools.wraps(f)
    def wrapper(*args, **kwargs):
        if args[0] in cache_.keys():
            return cache_[args[0]]
        result = f(*args, **kwargs)
        cache_[args[0]] = result
        return result
    return wrapper


# the new revenue is prices[i] + cut_rod_(n - i, prices) - cost
def cut_rod(n, prices, cost):

    @cache
    def cut_rod_(n, prices) -> int:
        if n == 0:
            return 0
        q = -999
        for i in range(1, n+1):
            p1 = prices[i] + cut_rod_(n - i, prices) - cost
            if p1 > q:
                q = p1

        return q
    r = cut_rod_(n, prices)

    return r


def show():
    cost = 2
    dp_table = [cut_rod(i, rod_price, cost)
                for i in range(0, len(rod_price))]

    print(f"cost {cost}")
    print("{:10s} {:10s} ".format("length", "renevnue"))
    for i, r in enumerate(dp_table):
        print(f"{i:<10d} {r:<10d}")


if __name__ == "__main__":
    show()
