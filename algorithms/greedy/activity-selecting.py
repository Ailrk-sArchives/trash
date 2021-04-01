s = [1, 3, 0, 5, 3, 5, 6, 8, 8, 2, 12]
f = [4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]

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


def act_selection(s, f):
    result = []
    n = len(f)
    i = 0
    for j in range(n):
        if s[j] >= f[i]:
            result.append((i, j))
            i = j
    return result


if __name__ == "__main__":

    r = act_selection(s, f)
    print(r)
