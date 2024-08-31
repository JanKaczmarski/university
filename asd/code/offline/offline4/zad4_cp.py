from zad4testy import runtests

def init_graph(G) -> list:
    arr = [[]]
    for edge in G:
        vertex1, vertex2, height = edge[0], edge[1], edge[2]
        x = max(vertex1, vertex2) + 1 - len(arr)
        if x > 0:
            arr.extend([[] for _ in range(x)])
        
        arr[vertex1].append((vertex2, height))
        arr[vertex2].append((vertex1, height))
    
    return arr


def Flight(L,x,y,t):
    G = init_graph(L)
    sol = False
    visited = [[False for _ in range(len(G))] for __ in range(len(G))]

    def travel(G, u, min_h, max_h):
        nonlocal sol
        
        # if solution already found
        if sol:
            return
        
        if u == y:
            sol = True
            return 
        
        # visit every neighbour if the height is appriopriate
        for v in G[u]:
            if not visited[u][v[0]]:
                visited[u][v[0]] = visited[v[0]][u] = True
                new_max = max(max_h, v[1])
                new_min = min(min_h, v[1])
                    
                if (new_max - new_min) <= 2 * t + 1:
                    travel(G, v[0], new_min, new_max)
                
    # visit every neigbour of starting point 
    for v in G[x]:
        travel(G, v[0], v[1] , v[1])
    
    return sol

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( Flight, all_tests = True )

#L = [(0,1,2000),(0,2,2100),(1,3,2050),(2,3,2300),(2,5,2300),(3,4,2400),(3,5,1990),(4,6,2500),(5,6,2100)]
#print(Flight(L, 0, 6, 60))

"""
Graf jest nieskierowany wiec cos tu nie gra
trzeba zrobic moze lepsza tablice sasiedztwa
"""

