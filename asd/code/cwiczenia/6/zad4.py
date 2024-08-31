"""
Kierownik chce przewiezc turystow, z miasta A do B. Mamy graf reprezentujacy 
polaczenia miedzy miastami. Kazda krawedz grafu jest znakowana max liczba pojemnosci
autobusu. Przewodnik ma przewiezc grupe k turystow. Przewodnik musi wyznaczyc wspolna
trasa dla wszystkich turystow, musi podzielic ich na grupki tak zeby kazda grupka nie
rozdzielala sie. Oblicz na ile najmniej grupek przewodnik musi podzielic turystow, aby kazdy
dostal sie z A do B. 
"""

# k = 100

# 1. szukamy maksymalnego drzewa rozpinajacego
# wielkosc grup to najmniejsza waga w tym drzewie

# 2. 


# Algorytm Kruskala
# edges list
from collections import deque

def findset(x, parent):
    if x != parent[x]:
        x = findset(parent[x], parent)
    return x

def unionset(x, y, parent, rank):
    x = findset(x, parent)
    y = findset(y, parent)
    
    if rank[x] > rank[y]:
        parent[y] = x
    else:
        parent[x] = y
        if rank[y] == rank[x]:
            rank[y] += 1

def kruskal_max(G, vertex_num):
    s_edges = sorted(G, key=lambda x: x[2], reverse=True)
    result = []
    parent = [i for i in range(vertex_num)]
    rank = [0 for _ in range(vertex_num)]
    
    for u, v, weight in s_edges:
        if findset(u, parent) != findset(v, parent):
            unionset(u, v, parent, rank)
            result.append((u, v, weight))
    
    return result
                
def edge_to_list(G, vertex_num):
    # (weight, vertex)
    result = [[] for _ in range(vertex_num)]
    for u, v, w in G:
        result[u].append((w,v))
        result[v].append((w, u))
    
    return result
    
    
def get_route(G, vertex_num, start, end):
    mst = edge_to_list(kruskal_max(G, vertex_num), vertex_num)
    visited = [False for _ in range(vertex_num)]
    parent = [None for _ in range(vertex_num)]
    parent_res = [float('inf') for _ in range(vertex_num)]
    
    
    def dfsVisit(G, u):
        visited[u] = True
        for w, v in G[u]:
            if not visited[v]:
                parent[v] = u
                parent_res[v] = min(parent_res[u], w)
                dfsVisit(G, v)

        return parent_res[v]
    
    
    return dfsVisit(mst, start)
    
G = [
    [0, 1, 10],
    [0, 4, 7],
    [1, 2, 15],
    [2,5,12],
    [5,7, 13],
    [7, 6, 15],
    [6, 3, 20],
    [6, 4, 13],
    [4, 3, 8],
    [3, 2, 9],
    [3, 7, 25]
    
]
vertex_num =8
print(get_route(G, vertex_num, 0, 7))
    
    