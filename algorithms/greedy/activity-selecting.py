sf = [(3, 5), (3, 8), (0, 6), (5, 9), (1, 4), (8, 11),
      (8, 12), (5, 7), (12, 14), (6, 10), (2, 13)]

# we first state the problem as a dp optimization problem
# then we realize it doesn't need to be dp, because each
# time when we need to make a decision, we only need to choose
# the optimal one, and the result will be proven to be optimal.

# we sort the activity based on their finished time. This ensure
# several properties:
# 1. the first task should always be considered. since it will
#    take the smallest time.
# 2. we can be sure the next non overlapping interval is the
#    best solution.


# primitive
def activity_selection(sf):
    sf1 = list(sorted(sf, key=lambda x: x[1]))
    __import__('pprint').pprint(sf1)
    s = [s_ for (s_, _) in sf1]
    f = [f_ for (_, f_) in sf1]
    result = []
    n = len(f)
    i = 0
    for j in range(n):
        if s[j] >= f[i]:
            result.append((i, j))
            i = j
    return result


# 1. Select the last to start;
# Selecting the last activity to start is isomorphic to selecting the first
# activity to finish.
# For the first activity to start case, we have a set of intervals
# {a1, a2 ... an }, with ai = [si, fi), and we find the last activity that is
# compatible with previous previous activities. If we reverse the order of
# si and fi, and create an interval {b1, b2 ... bn} with bi = [fi, si].
# We can solve this reversed problem with the same algorithm and get exactly
# the same result.

def activity_selection_reverse(sf):
    sf1 = list(reversed(sorted(sf, key=lambda x: x[0])))
    __import__('pprint').pprint(sf1)
    f = [s_ for (s_, _) in sf1]
    s = [f_ for (_, f_) in sf1]

    result = []
    n = len(f)
    i = 0
    for j in range(n):
        if s[j] <= f[i]:
            result.append((i, j))
            i = j
    return result

# 2.
#   - least duration from compatible activities
#       s  0  4  5
#       f  5  7  9
#      (4, 7) has the least duration 3. So it will be picked directly as
#      solution.
#      but by choosing (0, 5) (5, 9) we can fit 2 tasks.
#   - overlaps the fewest other remaining activities
#       s  0 5 6 7 8 12
#       f  6 7 8 8 9 13
#       (12, 13) has the least overlap and will be choosen at the beginning.
#       But it's clearly not optimal
#   - selecting the compatible remaining activity with the earliest start time.
#       s  0   4  5
#       f  11  7  9
#     once (0, 11) is choosen no other tasks can fit anymore


if __name__ == "__main__":

    print("last start solution")
    r = activity_selection(sf)
    print(f"result: {r}")

    print()

    print("first finished")
    r1 = activity_selection_reverse(sf)
    print(f"result: {r1}")
