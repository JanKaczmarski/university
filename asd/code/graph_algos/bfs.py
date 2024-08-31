from collections import deque

"""
Both algos are working
"""

# for matrix representation
def bfs(G, s):
    n = len(G)
    # distance from s
    distance = [-1 for _ in range(n)]
    visited = [False for _ in range(n)]
    parent = [None for _ in range(n)]
    q = deque()
    distance[s] = 0
    visited[s] = True
    parent[s] = None
    q.append(s)
    while len(q) > 0:
        v = q.popleft()
        for i in range(n):
            if G[v][i] == 1 and not visited[i]:
                visited[i] = True
                distance[i] = distance[v] + 1
                parent[i] = v
                q.append(i)
    return distance, visited, parent


# arr of neighbours
def bfsv2(G, s):
    n = len(G)
    distance = [-1 for _ in range(n)]
    visited = [False for _ in range(n)]
    parent = [None for _ in range(n)]
    q = deque()
    distance[s] = 0
    visited[s] = True
    parent[s] = None
    q.append(s)
    while len(q) > 0:
        v = q.popleft()
        for child in G[v]:
            if not visited[child]:
                visited[child] = True
                distance[child] = distance[v] + 1
                parent[child] = v
                q.append(child)
    
    return distance, visited, parent


if __name__ == '__main__':
    A = [
        [0, 1, 1, 0],
        [1, 0, 0, 1],
        [1, 0, 0, 0],
        [0, 1, 0, 0]
    ]
    a = [
        [1, 2],
        [0,3],
        [0],
        [1]
    ]

    