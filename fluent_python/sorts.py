import bisect
import random
from typing import List

SIZE = 15

fruits = ['grape', 'raspberry', 'apple', 'banana', 'pineapple-pie']
print(sorted(fruits, key=len))
fruits.sort()
print(fruits)

print('use bisect perfrom table lookup')


def grade(score, breakpoints=[60, 70, 80, 90], grades='FDCBA'):
    i = bisect.bisect(breakpoints, score)
    return grades[i]


print([grade(score) for score in [33, 99, 77, 70, 89, 90, 100]])

# use insort to maintain the order of sequence while inserting.
print('use bisect insort')
random.seed(1729)

alist: List[int] = []
for i in range(SIZE):
    new = random.randrange(SIZE * 2)
    bisect.insort(alist, new)
    print('{:2d} ->'.format(new), alist)
