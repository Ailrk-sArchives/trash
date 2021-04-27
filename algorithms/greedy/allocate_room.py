schedules = [(1, 4), (2, 5), (5, 7), (10, 12), (6, 9), (13, 14)]

# each time we have a overlapping we know a new room is needed.


def sort(xs):
    return list(sorted(xs, key=lambda x: x[0]))


def allocate(xs):
    sorted_xs = sort(xs)
    result = 0

    i = 0
    for j in range(len(schedules)):
        if sorted_xs[j][0] < sorted_xs[i][1]:
            i = j
            result += 1
    return result


allocate(schedules)
