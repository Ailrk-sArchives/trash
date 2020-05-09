def make_avg():
    count = 0
    total = 0

    def avg(n):
        nonlocal count, total  # declare var as free variable.
        count += 1
        total += n
        return total / count
    return avg


if __name__ == "__main__":
    avg = make_avg()
    exprs = """
        avg.__code__.co_freevars
        avg.__closure__
        avg.__closure__[0].cell_contents
    """

    for expr in exprs.split():
        print(expr, eval(expr))

    print('avg(100)', avg(100))
    for expr in exprs.split():
        print(expr, eval(expr))

    print('avg(200)', avg(200))
    for expr in exprs.split():
        print(expr, eval(expr))

    print('avg(999)', avg(999), '\n')
    for expr in exprs.split():
        print(expr, eval(expr))


