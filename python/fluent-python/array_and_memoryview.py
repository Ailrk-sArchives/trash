import array
import random
from pprint import pprint

"""
array:   c type array
bytes:   can convert to array
pickle:  more flexible in terms of storage
array and to file and from file, from byte directly.
memoryview: handle slice of arrays without copy bytes.
"""

#######################################################################
# array with different c types
rand_array_h = array.array('h')
rand_array_i = array.array('i')

byte = b'asjdfoiasjdfoiajsdfajsdlfasdjlfajsdlfjasdf09'
rand_array_h.frombytes(byte)
rand_array_i.frombytes(byte)

print('an h array from bytes {}\n'.format(byte))
pprint(rand_array_h)
print("---------------------------------------------->")
print('an i array from bytes {}\n'.format(byte))
pprint(rand_array_i)

#######################################################################
# write array into file and form array from file
floats_array = array.array('d', (random.random() for _ in range(10**7)))
floats_array2 = array.array('d')
with open('floats.bin', 'wb') as fp:
    floats_array.tofile(fp)

with open('floats.bin', 'rb') as fp:
    floats_array2.fromfile(fp, len(floats_array))


print("---------------------------------------------->")
print('-> floats_array [20:22] {}'.format(floats_array[20:22]))
print('-> floats_array2 [20:22] {}'.format(floats_array2[20:22]))

print("---------------------------------------------->")
floats_array2.byteswap()
print('-> floats_array2 swaped [20:22] {}'.format(floats_array2[20:22]))
floats_array2 = array.array(floats_array2.typecode, sorted(floats_array2))
print('-> floats_array2 sorted [20:22] {}'.format(floats_array2[20:22]))
#######################################################################
# change array (or bytes) dynamically and efficiently with memoryview
print("---------------------------------------------->")
print('memoryview example:')
numbers = array.array('h', [-2, -1, 0, 1, 2])   # short
memv = memoryview(numbers)
memv_oct = memv.cast('B')                       # unsigned char
print('array <numbers>:', numbers)
print('memview in short', list(memv))
print('memoryview in char', list(memv_oct))
memv_oct[5] = 30
print('array <numbers> modified by memoryview:', numbers)



