class UpperAttrMetaCls(type):
    def __new__(cls, name, bases, attrs):
        # __new__ will be called before __init__
        uppercaseattrs = {}
        for name, val in attrs.items():
            if not name.startswith("--"):
                uppercaseattrs[name.upper()] = val
            else:
                uppercaseattrs[name] = val
        return super().__new__(cls, name, bases, uppercaseattrs)


class SampleA(metaclass=UpperAttrMetaCls):
    name = 10

    def __init__(self):
        print("created a SampleA, with Metacls UpperAttrMetaCls")


def monkey_speak():
    print("ook ook eee eee eee!")


if __name__ == "__main__":
    SampleA.speak = monkey_speak  # BONUS: monkey patching.
    a = SampleA()

    print(a.NAME)  # use metaclass changed the attri names.
    a.speak()
