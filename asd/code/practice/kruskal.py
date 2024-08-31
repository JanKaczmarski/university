
def kruskal(edges, n):
    edges.sort(key=lambda x: x[2])
    parent = [i for i in range(n)]
    rank = [0 for _ in range(n)]
    result = []
    total = 0
    
    def find_set(x):
        nonlocal parent
        while x != parent[x]:
            x = find_set(parent[x])
        
        return x
    
    def union_set(x, y):
        nonlocal parent, rank
        x = find_set(x)
        y = find_set(y)

        if rank[x] > rank[y]:
            parent[y] = x
        else:
            parent[x] = y
            if rank[x] == rank[y]:
                rank[y] += 1
        
    for u, v, w in edges:
        if find_set(u) != find_set(v):
            total += w
            result.append((u, v, w))
            union_set(u, v)
    
    return result, total
    
if __name__ == '__main__':
    G = [(0, 1, 3), (0, 2, 5), (1, 3, 3), (2, 3, 2), (2, 4, -1), (1, 4, 2)]
    print(kruskal(G, 5))
    