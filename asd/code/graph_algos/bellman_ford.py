from graphs_data import GraphTestUnit

# Adjacency List with weights (weight, vertex)
def bellman_ford(G: list[list[tuple[int, int]]], s: int) -> tuple[list[int], list[int]]:
    """Shortest paths in graph, when negative distance values can be used"""
    n = len(G)
    distance = [float('inf')] * n
    parent = [None] * n
    distance[s] = 0
    
    for _ in range(n - 1):
        for u in range(n):
            for weight, v in G[u]:
                if distance[v] > distance[u] + weight:
                    distance[v] = distance[u] + weight
                    parent[v] = u
    
    for u in range(n):
        for weight, v in G[u]:
            if distance[v] > distance[u] + weight:
                return [], []
    
    return distance, parent


if __name__ == '__main__':
    # Graph: [vertex: [(weight, vertex)]]
    G1 = [
        [(5, 1), (3, 2)],  # Vertex 0
        [(6, 3)],          # Vertex 1
        [(2, 1), (7, 3)],  # Vertex 2
        [],                # Vertex 3
    ]
    G2 = [
        [(1, 1)],  # Vertex 0
        [(2, 2)],  # Vertex 1
        [],        # Vertex 2
    ]
    G3 = [
        [(2, 1)],
        [(-8, 2)],
        [(1, 0)]
    ]
    graphs = [G1, G2, G3]
    sources = [0, 0, 0]
    
    for i in range(len(graphs)):
        G = graphs[i]
        source = sources[i]
        
        testUnit = GraphTestUnit(G, 'l')
        assert bellman_ford(G, source) == testUnit.bellmanFord(source)
    
    print("Assertions correct!")