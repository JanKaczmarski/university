# Jan Kaczmarski
# Przygotwoawc graf, zeby mozna go bylo latwo wrzucic do algorytmu dijkstry
# za pomoca dijkstry szukamy najkrotesz sciezki miedzy a i b
# zwracamy wartosc jesli droga istnieje w przeciwnym wypadku zwracam None
# zlozonosc czasowa: O(S^2 + E + (E + S)logV )

from zad5testy import runtests
from queue import PriorityQueue


def dijkstra(G, s, e):
    n = len(G)
    parent = [None] * n
    distance = [float('inf')] * n
    Q = PriorityQueue()
    distance[s] = 0
    Q.put((0, s))
    while not Q.empty():
        u = Q.get()[1]
        for w, v in G[u]:
            if distance[v] > distance[u] + w:
                distance[v] = distance[u] + w
                parent[v] = u
                Q.put((distance[v], v))
                
            # if there is connection return distance
            if u == e:
                return distance[u], parent
    
    # if there is no connection between s and e
    return distance, parent


def init_graph(E, S, n):
    G = [[] for _ in range(n)]
    s = len(S)
    for edge in E:
        v, u, distance = edge
        G[v].append((distance, u))
        G[u].append((distance, v))
    
    for i in range(s):
        for j in range(i + 1, s):
            G[S[i]].append((0, S[j]))
            G[S[j]].append((0, S[i]))
    
    return G


def spacetravel( n, E, S, a, b ):
    G = init_graph(E, S, n)
    distance, parent = dijkstra(G, a, b)
    if type(distance) not in (int, float):
        return None
    
    return distance

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( spacetravel, all_tests = True )

