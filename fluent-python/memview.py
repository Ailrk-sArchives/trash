import time
"""
memoryview slice buffer protocol object without slice it.
"""

"""
    pure byte 100000 0.135453462600708
    pure byte 200000 0.5963990688323975
    pure byte 300000 1.4644675254821777
    pure byte 400000 2.7435734272003174
    memoryview 100001 0.007353782653808594
    memoryview 200000 0.014733552932739258
    memoryview 300000 0.02210545539855957
    memoryview 400000 0.029502391815185547
"""


def test(with_memview=False):
    scales = (100000, 200000, 300000, 400000)
    for n in scales:
        data = bytes(('x' * n).encode('ascii'))
        start = time.time()
        if with_memview:
            name = 'memoryview'
            b = memoryview(data)
        else:
            name = 'pure byte'
            b = data
        while b:
            b = b[1:]
        print(f"{name}", n, time.time() - start)


if __name__ == "__main__":
    test()
    test(with_memview=True)
