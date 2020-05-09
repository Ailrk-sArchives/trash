"""
You don't want to update a dictionary while loop through its key
because when you changed some elements the py iterpreter might
decide to resize the dict, which can cause a big temporay overhead.

Better way to do it is to loop through dict, and record all elements
need to be updated in another dict, update them outside of the loop.

This can decrease the frequency of dict migration in memory.
"""

from random import random, randint

d = {randint(0, 10**6): random() for _ in range(10**6)}
print({key: val for key, val in d.items() if key % 219989 == 0})
# update d:

d.update({key: randint(10**7, 10**8) for key, _ in d.items() if key % 219989 == 0})
print({key: val for key, val in d.items() if key % 219989 == 0})
