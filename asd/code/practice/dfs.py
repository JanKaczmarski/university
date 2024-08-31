

def dfs(G):
    n = len(G)
    visited = [False for _ in range(n)]
    parent = [None for _ in range(n)]
    time = -1
    times = [-1 for _ in range(n)]
    
    def dfsVisit(G, u):
        nonlocal visited, parent, time, times
        visited[u] = True
        
        for v in G[u]:
            if not visited[v]:
                parent[v] = u
                dfsVisit(G, v)

        time += 1
        times[u] = time
    
    for u in range(n):
        if not visited[u]:
            dfsVisit(G, u)
    
    return visited, parent, times
            
            
if __name__ == '__main__':
    a = [
        [1, 2],
        [0, 3, 4],
        [0, 3, 5],
        [1, 2, 6],
        [1, 3, 7],
        [2, 6],
        [3, 5, 7],
        [4, 6, 8],
        [7]
    ]
    
    print(dfs(a))
