import bisect
import sys

"""
a binary insertion lib.
If you have a sorted list and want to keep it in order when interting
new elements, insert througth bisect.insort
"""

HAYSTACK = [1, 4, 5, 6, 8, 12, 15, 20, 21, 23, 23, 26, 29, 30]
NEDDLES = [0, 1, 2, 5, 8, 10, 22, 23, 29, 30, 31]

ROW_RMT = '{0:2d} @ {1:2d}    {2}{0:<2d}'


def demo(bisect_fn):
    for needle in reversed(NEDDLES):
        pos = bisect_fn(HAYSTACK, needle)
        offset = pos * '  |'
        print(ROW_RMT.format(needle, pos, offset))


if __name__ == '__main__':
    if sys.argv[-1] == 'left':
        bisect_fn = bisect.bisect_left
    else:
        bisect_fn = bisect.bisect

    print("DEMO:", bisect_fn.__name__)
    print("haystack ->", " ".join('%2d' % n for n in HAYSTACK))
    demo(bisect_fn)

