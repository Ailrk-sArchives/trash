class Bottle1:
    """
    getter a nd setter
    """

    volume_: int = 0

    @property
    def volume(self):
        return self.volume_

    @volume.setter
    def volume(self, v: int):
        print("set bottle")
        self.volume_ = v


class Bottle2:
    """
    Getter but no setter
    """

    volume_: int = 0

    @property
    def volume(self):
        return self.volume_


class Bottle3:
    """
    Getter but no setter
    """

    volume: int = 0

    def __setattr__(self, name, value):
        if name == "volume":
            print("set bottle")
        super().__setattr__("volume", value)


b1 = Bottle1()
b2 = Bottle2()
b3 = Bottle3()
