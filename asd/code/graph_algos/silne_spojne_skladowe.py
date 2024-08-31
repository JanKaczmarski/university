
def dfs_time_processed(G, ):
    n = len(G)
    visited = [False for _ in range(n)]
    # idx is a time value
    # in cell we have the vertex processed in this time
    time= -1
    time_p = [-1 for _ in range(n)]
    
    def dfsVisit(G, u):
        nonlocal visited, time, time_p
        visited[u] = True
        
        for v in G[u]:
            if not visited[v]:
                dfsVisit(G, v)
                
        time += 1
        time_p[time] = u
        
    for u in range(n):
        if not visited[u]:
            dfsVisit(G, u)
    
    return time_p
        
    
def reverse_graph(G):
    n = len(G)
    result = [[] for _ in range(n)]
    for u in range(n):
        for v in G[u]:
            result[v].append(u)
    
    return result


def sol(G):
    n = len(G)
    roots = []
    visited = [False for _ in range(n)]
    time_processed = dfs_time_processed(G)
    reversed_g = reverse_graph(G)
    
    def dfsVisit(G, u):
        nonlocal visited
        visited[u] = True
        for v in G[u]:
            if not visited[v]:
                dfsVisit(G, v)
    
    for i in range(n - 1, -1, -1):
        u = time_processed[i]
        if not visited[u]:
            roots.append(u)
            dfsVisit(reversed_g, u)
            
    return roots
    
    
if __name__ == '__main__':
    a = [
        [1],
        [2],
        [0, 3],
        [4],
        [5],
        [3]
    ]
    
    print(sol(a))
            
    