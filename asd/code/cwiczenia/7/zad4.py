"""
wagi sa z zakresu {1,...,licznosc E}
zaproponuj algorym ktory dla danych wierzcholkow s i v oblicza minimalna
sciezke po krawedziach o malejacych wagach
"""

"""
1. Posortwoac krawedzie po wadze DESC;
2. wybieramy te krwaedzie ktore nam zmienija odlegosc (cos wnosza/ Nie ma inf na obu
koncach)
3. Z wybranych krawedzi uzywamy algorytmu Bellmana-Forda
"""

# edges list
# (u, v, weight)
def bellman_ford(edges, vertex_num, start, end):
    distance = [float('inf') for _ in range(vertex_num)]
    distance[start] = 0
    
    for _ in range(vertex_num - 1):
        for u, v, weight in edges:
            # relax
            distance[v] = min(distance[v], distance[u] + weight)

    for u, v, weight in edges:
        if distance[v] > distance[u] + weight:
            # there is a negative cycle
            return -1
                
    return distance[end]


def zad4(edges, vertex_num, start, end):
    distance = [float('inf') for _ in range(vertex_num)]
    distance[start] = 0
    s_edges = sorted(edges, key=lambda x: x[2], reverse=True)
    valid_edges = []
    # get all edges that can create a path and are in DESC order
    for u, v, weight in s_edges:
        if distance[u] != float('inf') or distance[v] != float('inf'):
            distance[u] = distance[v] = 0
            valid_edges.append((weight, u, v))
            
    
    return bellman_ford(edges, vertex_num, start, end)
    

graph = [
    [0, 1, 20],
    [1, 2, 10],
    [2, 3, 1],
    [2, 4, 5],
    [4, 5, 3],
    [3, 5, 2]
]
vertex_num = 6
print(zad4(graph, vertex_num, 0, 5))

