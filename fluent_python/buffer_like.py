import array
import struct


"""
byte is the underline representation of str.
python make the difference between char to unicode exlicit.
"""

numbers = array.array('b', [68, 69, 77])
numbyte = bytes(numbers)        # all buffer like sequence can be converted ty bytes.
numbytearry = bytearray(numbyte)
numbytearry[2] = 98

# create bytes from buffer like seq will copy bytes.
print('-> An array:                  ', numbers)
print('-> Turn the array into bytes: ', numbyte)
print('-> turn bytes into bytearray (mutable): ', numbytearry)
print('-> slice of bytearray return bytearray: ', numbytearry[-1:])
print('-> convert from hex to bytes: ', bytes.fromhex('31 4B CE'))


"""
struct: handle parse packed byte into typle and the opposite.
"""
print()
fmt = '<3s3sHH'
with open('./68.80.gif', 'rb') as fp:
    img = memoryview(fp.read())
    header = img[:10]   # slice a memoryview will not copy bytes.
    print("header of gif represent in bytes ", bytes(header))
    print("header unpacked: ", struct.unpack(fmt, header))

