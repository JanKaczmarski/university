from queue import PriorityQueue

def djikstra(G, start):
    n = len(G)
    distance = [float('inf') for _ in range(n)]
    parent = [None for _ in range(n)]
    Q = PriorityQueue()
    
    distance[start] = 0
    Q.put((0, start))
    
    while not Q.empty():
        _, u = Q.get()
        for weight, v in G[u]:
            if distance[v] > distance[u] + weight:
                distance[v] = distance[u] + weight
                parent[v] = u
                Q.put((distance[v], v))
    
    return distance, parent

if __name__ == '__main__':
    a = [
        [(3, 1), (6, 2)],
        [(3, 0), (2, 3), (7, 4)],
        [(6, 0), (1, 3), (12, 5)],
        [(2, 1), (1, 2), (3, 6)],
        [(7, 1), (1, 3), (3, 7)],
        [(12, 2), (5, 6)],
        [(3, 3), (5, 5), (4, 7)],
        [(3, 4), (4, 6), (2, 8)],
        [(2, 7)]
    ]
    
    print(djikstra(a, 0))
    
    