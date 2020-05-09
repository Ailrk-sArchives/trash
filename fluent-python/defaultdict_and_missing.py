import sys
import re
import collections
from typing import DefaultDict
WORD_RE = re.compile('\w+')

# Default dict
# list is called a default_factory
index: DefaultDict = collections.defaultdict(list)
with open('./pyzen.txt', 'r') as fp:
    for line_no, line in enumerate(fp):
        for match in WORD_RE.finditer(line):
            word = match.group()
            column_no = match.start()
            location = (line_no, column_no)
            index[word].append(location)

for word in sorted(index, key=str.upper):
    print(word, index[word])


# __missing__ method

class StrKeyDict(dict):

    def __missing__(self, key):
        # isinstance to avoid recursive call if a unkown key is passed in.
        if isinstance(key, str):
            raise KeyError(key)
        return self[str(key)]

    def get(self, key, default=None):
        # handle the default case when KeyError is triggered.
        try:
            return self[key]
        except KeyError:
            return default

    def __contains__(self, key):
        # to make behavior of `k in d` consistent.
        return key in self.keys() or str(key) in self.keys()


print("---------------------------------------------------------->")
sd = StrKeyDict([('2', 'haha'), ('4', 'hehe'), ('9', 'xixi')])
print(sd.get(10))
print(sd.get(2))


