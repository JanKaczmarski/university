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
            if u == e:
                return distance[u], parent
    
    return distance, parent


