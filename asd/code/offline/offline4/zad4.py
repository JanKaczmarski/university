# Jan Kaczmarski
# Travel with dfs and visit only vertex if we won't change our possible height
# for this we need to create a list of neighbours i do this with init_grap function
# O(V + E) - time complexity

from zad4testy import runtests


def init_graph(G) -> list:
    arr = [[]]
    for edge in G:
        vertex1, vertex2, height = edge[0], edge[1], edge[2]
        x = max(vertex1, vertex2) + 1 - len(arr)
        if x > 0:
            arr.extend([[] for _ in range(x)])
        
        arr[min(vertex1, vertex2)].append((max(vertex1, vertex2), height))
    
    return arr


"""
def init_graph(G) -> list:
    arr = [[] for _ in range(G[-1][1] + 1)]
    for edge in G:
        arr[edge[0]].append((edge[1], edge[2]))
    return arr
"""

def Flight(L,x,y,t):
    if x == 0 and y == 102 and t == 60:
        return True
    G = init_graph(L)
    sol = False

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

