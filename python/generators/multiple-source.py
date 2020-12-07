"""
Generator dp for multiple sources.
"""
from typing import List, Generator


def concatenate(sources: List[Generator]):
    for s in sources:
        for item in s:
            yield item



