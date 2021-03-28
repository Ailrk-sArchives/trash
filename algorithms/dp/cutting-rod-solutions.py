import functools
import typing as t
rod_price = [-1, 1, 5, 8, 9, 10, 17, 17, 20, 24, 30]

# cutting rod with solutions collected.


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


# return a tuple consist renvenue and the
# length of the first cut.
@cache
def cut_rod(n, prices) -> t.Tuple[int, int]:
    if n == 0:
        return 0, 0
    q = -999
    for i in range(1, n+1):
        p0, _ = cut_rod(n - i, prices)
        p1 = prices[i] + p0
        if p1 > q:
            s = i       # best thing to do is cut off length i
            q = p1
    return q, s


def show():
    dp_table = [cut_rod(i, rod_price)
                for i in range(0, len(rod_price))]

    print("{:10s} {:10s} {:10s} ".format("length", "renenue", "cutoff"))
    for i, (r, s) in enumerate(dp_table):
        print(f"{i:<10d}  {r:<10d}  {s:<10d}")


if __name__ == "__main__":
    show()

# length     renenue    cutoff
# 0           0           0
# 1           1           1
# 2           5           2
# 3           8           3
# 4           10          2
# 5           13          2
# 6           17          6
# 7           18          1
# 8           22          2
# 9           25          3
# 10          30          10
