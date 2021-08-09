from typing import Optional


class Singleton:
    class __OnlyOne:
        def __init__(self, arg):
            self.val = arg

        def __str__(self):
            return repr(self) + str(self.val)

    instance: Optional[__OnlyOne] = None

    def __init__(self, arg):
        if not Singleton.instance:
            Singleton.instance = Singleton.__OnlyOne(arg)
        else:
            Singleton.instance.val = arg

    def __getattr__(self, name):
        return getattr(self.instance, name)

    def __str__(self):
        return str(Singleton.instance)


if __name__ == "__main__":
    x = Singleton('sausage')
    print(x)
    y = Singleton('eggs')
    print(y)
    z = Singleton('spam')
    print(z)
    print(x)
    print(y)








