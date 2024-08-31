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


# matrix representaion
def euler_cycle(G):
    sol = []
    sub_cycle = []
    n = len(G)
    # O(n) time complexity
    edges = G.copy()
    
    new_cycle = False


    def dfsVisit(G, u):
        nonlocal edges, new_cycle, sol, sub_cycle
        for i in range(n):
            if edges[u][i] == 1:
                if new_cycle:
                    edges[u][i] = edges[i][u] = 0
                    
                else:
                    edges[u][i] = edges[i][u] = 0
                    dfsVisit(G, i)
        
        sol.append(u)
        new_cycle = True

    
    dfsVisit(G, 0)

    # cycle should be 1 element longer than initial list
    # we start with 0 and end with 0 edge
    if len(sol) != n + 1:
        # if there is no euler cycle return []
        return []
    
    return sol


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
    c = [
        [0, 1, 0, 1],
        [1, 0, 1, 0],
        [0, 1, 0, 1],
        [1, 0, 1, 0]
    ]
    print(euler_cycle(c))