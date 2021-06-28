# https://www.educative.io/blog/crack-amazon-coding-interview-questions#questions


######################################################
# note range is [) interval
def find_missing(input):
    return sum(range(1, len(input) + 2)) - sum(input)

xs = [3, 7, 1, 2, 8, 4, 5]
assert (find_missing(xs)) == 6, "wrong"


######################################################
def find_sum_of_two(input, val):
    s = set(input)
    return any(map(lambda x: val - x in s, input))

xs = [5, 7, 1, 2, 8, 4, 3]
assert find_sum_of_two(xs, 10)
assert !find_sum_of_two(xs, 19)


######################################################
def merge_sorted(head1, head2):
    return head1

