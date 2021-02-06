"""
There is no method overload in python.
So we need to use dispatch funtion to achieve the same
goal.

use singedispatch to from a generic function.
"""

import functools
from collections import abc
import numbers
import html


@functools.singledispatch
def htmlize(obj):
    content = html.escape(repr(obj))
    return '<pre>{}</pre>'.format(content)


@htmlize.register(str)
def htmlize_1(text):
    content = html.escape(text).replace('\n', '<br>\n')
    return '<p>{}</p>'.format(content)


@htmlize.register(numbers.Integral)
def htmlize_2(n):
    return '<pre>{0} (0x{0:x})</pre>'.format(n)


@htmlize.register(tuple)
@htmlize.register(abc.MutableSequence)
def htmlize_3(seq):
    inner = '</li>\n</li>'.join(htmlize(item) for item in seq)
    return '<ul>\n<li>' + inner + '</li>\n</ul>'


if __name__ == "__main__":
    print(htmlize({1, 2, 3}))
    print(htmlize(abs))
    print(htmlize('a string with \n'))
    print(htmlize(213))
    print(htmlize(['asdsa\n', 123, [n for n in range(4)]]))

