from queue import PriorityQueue

def prim(G):
    n = len(G)
    parent = [None for _ in range(n)]
    weights = [float('inf') for _ in range(n)]
    processed = [False for _ in range(n)]
    pq = PriorityQueue()
    pq.put((0, 0))
    
    while not pq.empty():
        _, u = pq.get()
        
        if processed[u]: continue
        
        processed[u] = True
        
        for e_weight, v in G[u]:
            if not processed[v] and e_weight < weights[v]:
                parent[v] = u
                weights[v] = e_weight
                pq.put((e_weight, v))
                
    return parent, weights
    
    
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
    
    print(prim(a))