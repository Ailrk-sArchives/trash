from typing import Optional, overload


@overload
def foo(x: None) -> None:
    ...


@overload
def foo(x: int) -> int:
    ...


def foo(x: Optional[int]) -> Optional[int]:
    if x is None:
        return None
    return x


