import collections
from typing import Dict
import re
import sys

######################################################
# dict comprehension
DIAL_CODES = [
    (86, 'China'),
    (91, 'India'),
    (1, 'United States'),
    (62, 'Indonesia'),
    (55, 'Brazil'),
    (92, 'Pakistan'),
    (880, 'Bangladesh'),
    (7, 'Russia'),
    (81, 'japan'),
]

contry_code = {contry: code for code, contry in DIAL_CODES}

print(contry_code)

######################################################
# by getting value of mapping with .get you can set a default val if the key
# doesn't exist.
print(contry_code.get('Moon', -1))

######################################################
# use setdefault

WORD_RE = re.compile('\w+')

index: Dict = {}
with open('./pyzen.txt', 'r') as fp:
    for line_no, line in enumerate(fp, 1):
        for match in WORD_RE.finditer(line):
            word = match.group()
            column_no = match.start() + 1
            location = (line_no, column_no)
            index.setdefault(word, []).append(location)  # setdefault return the value

for word in sorted(index, key=str.lower):
    print(word, index[word])


######################################################
# ChainMap to mimic python name lookup.
pylookup = collections.ChainMap(locals(), globals(), vars())


######################################################
# Counter
print("Counter-------------------------------------------------->")
ct = collections.Counter('aadsasdasdasdasdsadsdsdasd')
print(ct)
ct.update('aaaaaazzzzzsp')
ct = collections.Counter('aadsasdasdasdasdsadsdsdasd')
print(ct.most_common(2))


######################################################
# UserDict
# It is designed to be subclassed.
print("UserDict-------------------------------------------------->")


class StrKeyDict(collections.UserDict):
    def __missing__(self, key):
        if isinstance(key, str):
            raise KeyError(key)
        return self[str(key)]

    def __contains__(self, key):
        return str(key) in self.data

    def __setitem__(self, key, item):
        self.data[str(key)] = item


