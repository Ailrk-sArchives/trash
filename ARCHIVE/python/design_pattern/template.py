class Framework:
    def __init__(self):
        self.__template_method()

    def __template_method(self):
        for i in range(5):
            self.customize1()
            self.customize2()


class Instance(Framework):
    def customize1(self):
        print("meowmeow")

    def customize2(self):
        print("ho ho ho")

