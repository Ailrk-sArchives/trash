import weakref
import gc


class Dummy:
    """
    Dummy with visible lifecycle.
    """
    def __init__(self, name):
        self.obj = None
        self.name = name

    def __del__(self):
        print('destroyed')

    def store(self, obj):
        self.obj = obj

    def show(self):
        print(self.obj)


def weakr():
    print('start ---')
    a = Dummy('1')
    r = weakref.ref(a)
    r().show()
    print('ref deleted')
    del a

    print('try to call weak ref')
    try:
        r().show()
    except AttributeError:
        print('object is collected')
    del r

    print('end ---')


def weakRefToCache():
    """
    its to cache object for a little longer.
    not really cache
    """
    d = Dummy('Big Object')

    w = weakref.WeakValueDictionary()
    w['d'] = d
    d.store('turturtur')

    w['d'].show()
    del d
    gc.collect()



if __name__ == "__main__":
    weakr()
    print('====================')
    weakRefToCache()
