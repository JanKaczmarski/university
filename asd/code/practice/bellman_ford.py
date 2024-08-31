

def bellman_ford(G, start):
    n = len(G)
    distance = [float('inf') for _ in range(n)]
    parent = [None for _ in range(n)]
    
    distance[start] = 0
    
    for _ in range(n - 1):
        for u in range(n):
            for w, v in G[u]:
                if distance[v] > distance[u] + w:
                    distance[v] = distance[u] + w
                    parent[v] = u
    
    # detect negative cycles in graph
    for u in range(n):
        for w, v in G[u]:
            if distance[v] > distance[u] + w:
                return None
            
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
    
    print(bellman_ford(a, 0))
    