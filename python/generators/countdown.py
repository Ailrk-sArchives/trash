
class CountDown:
    """ countdown iterator """
    def __init__(self, start):
        self.count = start

    def __iter__(self):
        return self

    def __next__(self):
        if self.count <= 0:
            raise StopIteration
        r = self.count
        self.count -= 1
        return r


def desugar_countdown(start, callback):
    """ desugar for loop over iterator """
    it = iter(CountDown(start))
    while 1:
        try:
            x = next(it)
            callback(x)
        except StopIteration:
            break


def countdown_gen(n):
    """ countdown generator """
    # Generator is a more convinent way of writing iterator
    while n > 0:
        yield n
        n -= 1


def countdown_generator_expression(n):
    return (n for n in range(n, 0, -1))


if __name__ == "__main__":
    # for loop over iter
    print('for loop over iter')
    for i in CountDown(5):
        print(i)

    # desugar for loop over iter
    print('desuager for loop over iter')
    desugar_countdown(5, print)

    # countdown generator
    print('countdown generator')
    for n in countdown_gen(5):
        print(n)

    # countdown_generator_expression
    print('countdown generator expression')
    for n in countdown_generator_expression(5):
        print(n)
