def topology_sort(G):
    n = len(G)
    visited = [False for _ in range(n)]

    result = []
    
    def dfsVisit(G, u):
        nonlocal visited, result
        visited[u] = True
        
        for v in G[u]:
            if not visited[v]:
                dfsVisit(G, v)

        result.append(u)
    
    for u in range(n):
        if not visited[u]:
            dfsVisit(G, u)
    
    return result[::-1]



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
    print(topology_sort(a))