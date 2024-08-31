"""
stwierdzic czy istnieje sciezka hamiltona w acyklicznym grafie skierowanym
"""

# 1. Topology sort
# if connection with every neighbour we have cycle

"""
[0,1,2,3,5,4]
if connection with 0 and 1, 1 and 2, 2 and 3, 3 and 5, 5 and 4
"""

# adjacency list
def top_sort(G):
    n = len(G)
    visited = [False for _ in range(n)]
    result = [-1 for _ in range(n)]
    idx = n - 1

    def dfs_visit(G, u):
        nonlocal visited, idx, result
        for v in G[u]:
            if not visited[v]:
                visited[v] = True
                dfs_visit(G, v)
        
        result[idx] = u
        idx -= 1
    
    for u in range(n):
        if not visited[u]:
            dfs_visit(G, u)
            
    return result
    
def hamilton(G):
    result = top_sort(G)
    n = len(G)
    for i in range(n - 1):
        if result[i + 1] not in G[result[i]]:
            return False
    
    return True
    
