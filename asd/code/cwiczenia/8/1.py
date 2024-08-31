"""
Kruskal algorithm
"""

G = [(0, 1, 3), (0, 2, 5), (1, 3, 3), (2, 3, 2), (2, 4, -1), (1, 4, 2)]
n = len(G)
par = [i for i in range(n)]
rank = [0 for _ in range(n)]

def findset(x):
    if x != par[x]:
        par[x] = findset(par[x])
    return par[x]

def unionset(x, y):
    x = findset(x)
    y = findset(y)
    
    if rank[x] > rank[y]:
        par[y] = x
    else:
        par[x] = y
        if rank[y] == rank[x]:
            rank[y] += 1
            

def kruskal(edges):
    # edges are sorted !!!
    arr = []
    weight = 0
    for u, v, w in edges:
        if findset(u) != findset(v):
            unionset(u, v)
            weight += w
            arr.append((u, v, w))
            
    return arr, weight
            
print(kruskal(G))