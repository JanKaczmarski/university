
# neighbour array
def dfs(G):
    time = 0
    n = len(G)
    visited = [False for _ in range(n)]
    parent = [None for _ in range(n)]
    time_v = [-1 for _ in range(n)]

    def dfsVisit(G, u):
        nonlocal time, visited, time_v    
        time += 1
        time_v[u] = time
        visited[u] = True
        for v in G[u]:
            if not visited[v]:
                parent[v] = u
                dfsVisit(G, v)
        time += 1
    
    for u in range(n):
        if not visited[u]:
            dfsVisit(G, u)
        
    return parent, visited, time_v, time




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
    
    print(dfs(a))
            