from collections import deque

# Arr of neighbours
# WORKS?: Yes
def top_sort(G):
    n = len(G)
    visited = [False for _ in range(n)]
    parent = [None for _ in range(n)]
    sol = []

    def dfsVisit(G, u):
        nonlocal visited, parent, sol

        visited[u] = True
        for v in G[u]:
            if not visited[v]:
                parent[v] = u
                dfsVisit(G, v)

        sol.append(u)
    
    for u in range(n):
        if not visited[u]:
            dfsVisit(G, u)
        
    return sol[::-1]





if __name__ == '__main__':
    a = [
        [1, 4],
        [2],
        [3, 5],
        [2, 4],
        [0, 3, 5],
        [2, 4, 6],
        [5, 7],
        [6],
    ]
    b = [
        [1,2,5],
        [2,4],
        [],
        [],
        [3,6],
        [4],
        [0],
    ]
    #print(top_sort(b))
    