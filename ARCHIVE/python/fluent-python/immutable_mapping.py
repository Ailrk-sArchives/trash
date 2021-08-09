from types import MappingProxyType

"""
Use mappingproxy to create an immutable view of a mapping.
"""

d = {1: 'A'}
d_proxy = MappingProxyType(d)
print('->', d_proxy)
print('Now trying to change d, d_proxy will change dynamically.')
d[1] = 'C'
try:
    print('->', d_proxy)
    print('Now change d_proxy direcly. The action is not allowed.')
    d_proxy[1] = 'B'
except TypeError:
    print('MappingProxyType is immutable!')
