from queue import PriorityQueue

"""
Zaimplementuj algorytm dijkstry
"""

def dijkstra(G, a, b):
    n = len(G)
    parent = [None] * n
    distance = [float('inf')] * n
    Q = PriorityQueue()
    Q.put((0,a))
    distance[a] = 0
    
    while not Q.is_empty():
        u = Q.get()[1]
        for val, v in G[u]:
            if distance[v] > distance[u] + val:
                distance[v] = distance[u] + val
                parent[v] = u
                Q.put((distance[v], v))
            if u == b:
                return distance[u], parent
        
        # there is no path from a to b
        return distance, parent
    
    
