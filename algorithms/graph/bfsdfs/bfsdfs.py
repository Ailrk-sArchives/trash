import typing as t
# iterative algorithm for implementing bfs dfs.
# note: if it's a imperative programming language, it's better
#       just to use a stack or queue based implementation.
#       it's simple and robust, hard to make error.

# graph
graph_str = r"""
 0 - 1 - 3
     | / |
     2 - 5 - 7 - 8 - 9
     |   |   |
     4   6 - 10 - 11 - 12 - 13 - 14 - 15
"""

# a hash table with index as the key.
graph = [[1], [0, 2, 3], [1, 3, 4, 5],  # 0 - 2
         [1, 2, 5], [2], [2, 3, 6, 7],  # 3 - 5
         [5, 10], [5, 8, 10], [7, 9], [8],  # 6 - 9
         [6, 7, 11], [10, 12], [11, 13],  # 10 - 12
         [12, 14], [13, 15], [14]          # 13 - 15
         ]

# note this tree doesn't demonstrate the difference between bfs and dfs
# very well.
# the reason is the tree is skewd to the left, for out dfs it will pop the
# right node first, which is the short tree and easily hit the bottom.
tree_str = r"""
  tree
               0
             /   \
            1     2
           / \   / \
          3   4 5   6
         / \
        7   8
       /
      9
     /
    10
"""


tree = [[1, 2], [0, 3, 4], [0, 5, 6], [1, 7, 8],
        [1], [2], [2], [3, 9], [3], [7, 10],
        [9]
        ]

# for this tree dfs wil go all the way to the right subtree first
tree_str1 = r"""
  tree
               0
             /   \
            1     2
           / \   / \
          3   4 5   6
         / \         \
        7   8         11
       /               \
      9                 12
     /                   \
    10                    13
"""
tree1 = [[1, 2], [0, 3, 4], [0, 5, 6], [1, 7, 8],
         [1], [2], [2, 11], [3, 9], [3], [7, 10],
         [9], [6, 12], [11, 13], [12]
         ]


# create a traversal of the graph.
def dfs(graph, root) -> t.List[int]:
    visited = []
    stack = []
    visited.append(root)
    stack.append(root)
    while stack != []:
        v = stack.pop()
        for u in graph[v]:
            if u not in visited:
                visited.append(u)
                stack.append(u)
    return visited


def bfs(graph, root) -> t.List[int]:
    visited = []
    queue = []
    visited.append(root)
    queue.append(root)
    while queue != []:
        v = queue.pop(0)
        for u in graph[v]:
            if u not in visited:
                visited.append(u)
                queue.append(u)
    return visited


def main():
    print("graph test")
    print(graph_str)
    print("breath fisrt search (queue implemented), from 5")
    print(bfs(graph, 5))
    print("depth fisrt search (stack implemented), from 5")
    print(dfs(graph, 5))

    print("\n" + "=" * 30 + "\n")

    print("tree test")
    print(tree_str)
    print("breath fisrt search (queue implemented), from 0")
    print(bfs(tree, 0))
    print("depth fisrt search (stack implemented), from 0")
    print(dfs(tree, 0))

    print("\n" + "=" * 30 + "\n")

    print("deep tree test")
    print(tree_str1)
    print("breath fisrt search (queue implemented), from 0")
    print(bfs(tree1, 0))
    print("depth fisrt search (stack implemented), from 0")
    print(dfs(tree1, 0))


if __name__ == "__main__":
    main()

# result
r"""
graph test

 0 - 1 - 3
     | / |
     2 - 5 - 7 - 8 - 9
     |   |   |
     4   6 - 10 - 11 - 12 - 13 - 14 - 15

breath fisrt search (queue implemented), from 5
[5, 2, 3, 6, 7, 1, 4, 10, 8, 0, 11, 9, 12, 13, 14, 15]
depth fisrt search (stack implemented), from 5
[5, 2, 3, 6, 7, 8, 10, 11, 12, 13, 14, 15, 9, 1, 0, 4]

==============================

tree test

  tree
               0
             /   \
            1     2
           / \   / \
          3   4 5   6
         / \
        7   8
       /
      9
     /
    10

breath fisrt search (queue implemented), from 0
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
depth fisrt search (stack implemented), from 0
[0, 1, 2, 5, 6, 3, 4, 7, 8, 9, 10]

==============================

deep tree test

  tree
               0
             /   \
            1     2
           / \   / \
          3   4 5   6
         / \         \
        7   8         11
       /               \
      9                 12
     /                   \
    10                    13

breath fisrt search (queue implemented), from 0
[0, 1, 2, 3, 4, 5, 6, 7, 8, 11, 9, 12, 10, 13]
depth fisrt search (stack implemented), from 0
[0, 1, 2, 5, 6, 11, 12, 13, 3, 4, 7, 8, 9, 10]
"""
