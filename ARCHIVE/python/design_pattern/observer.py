"""
One subject can register multiple observers.
Each observers will be notified when subject changes its state.
"""


class Observable:
    def __init__(self):
        self.__observers = []

    def register_observer(self, observer):
        self.__observers.append(observer)

    def notify_observers(self, *args, **kwargs):
        for observer in self.__observers:
            observer.notify(self, *args, **kwargs)

    def __repr__(self):
        return str(id(self))

    def __str__(self):
        return str(id(self))


class Observer:
    def __init__(self, observable):
        observable.register_observer(self)

    def notify(self, observable, *args, **kwargs):
        print("observer", id(self), "Change", *
              args, *kwargs, "From", observable)


subject = Observable()
observer1 = Observer(subject)
observer2 = Observer(subject)
observer3 = Observer(subject)
subject.notify_observers('test')
