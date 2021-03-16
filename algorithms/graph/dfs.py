# also ok
# Why bfs dfs is repeatedly mentioned in graph related algorithm?
# because they are simple way to traverse over a graph and find some
# properties of it.
# Most graph related algorithms are little varaitions of bfs or dfs
# with different strategies.

# 0 -> 1
# 1 -> 0, 2
# 2 -> 1
#
# 0 - 1 - 3
#     | /
#     2

# a hash table with index as the key.
graph = [[1], [0, 2, 3], [1, 3], [1, 2]]


def dfs(v, visited):
    visited.append(v)
    for u in graph[v]:  # for adjacents.
        if u not in visited:
            visited = dfs(u, visited)
    return visited


def bfs(v, visited):
    NotImplemented
