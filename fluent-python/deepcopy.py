import copy

"""
The only mode of parameter passing is sharing.
"""


def badness_of_mutable_para():
    def f(a, b):
        # inplace assgin for mutable, create a new obj for immutable.
        a += b

    x, y = 1, 2
    a, b = [1, 2], [3, 4]
    u, t = (1, 2), (3, 4)
    print('primitive, ', x, y)
    print('mutable, ', a, b)
    print('immutable, ', u, t)
    f(x, y)
    f(a, b)
    f(u, t)
    print('primitive, ', x, y)
    print('mutable, ', a, b)
    print('immutable, ', u, t)


class Bus:

    def __init__(self, passengers=None):
        if passengers is None:  # defensive programming with mutable paras.
            self.passengers = []
        else:
            self.passengers = list(passengers)  # shallow copy!

    def pick(self, name):
        self.passengers.append(name)

    def drop(self, name):
        self.passengers.remove(name)


class HauntedBus:
    """Bad idea to make mutable types as default parameters"""

    def __init__(self, passengers=[]):
        # default list will be shared by sevaral instances
        # [] is in HauntedBus.__init__.__defaults__
        self.passengers = passengers

    def pick(self, name):
        self.passengers.append(name)

    def drop(self, name):
        self.passengers.remove()


def busmain():
    bus1 = Bus(['A', 'B', 'C', 'D'])
    bus2 = copy.copy(bus1)
    bus3 = copy.deepcopy(bus2)
    print('id of bus 1 2 3: {} {} {}'.format(*map(id, [bus1, bus2, bus3])))
    print('bus1 drop B')
    bus1.drop('B')
    print('bus1: ', bus1.passengers)
    print('bus2: ', bus2.passengers)
    print('id of bus 1 2 3: {} {} {}'.format(*map(id, [bus1, bus2, bus3])))
    print('bus3: ', bus3.passengers)


if __name__ == "__main__":
    badness_of_mutable_para()
    busmain()
