from collections import deque

"""
a double ended queue for fast interting and removing.
deque can be bounded.
"""

dq  = deque(range(10), maxlen=10)
print(dq)

dq.rotate(3)        # rotate right end elements of the deque to left.
print('right rotate 3', dq)

dq.rotate(-8)        # rotate right end elements of the deque to left.
print('left rotate 8', dq)

dq.extend([11, 22, 33])
print('push into dq', dq)
