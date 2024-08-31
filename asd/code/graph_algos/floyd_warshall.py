# adjacency matrix
# matrix is like this
# inf if no connection and weight if there is 
def floyd_warsh(G: list[list]):
    n = len(G)
    # matrix of distances
    distance_matrix = G.copy()
    # distance from itself is 
    for i in range(n):
        distance_matrix[i][i] = 0
        
    for k in range(n):
        for u in range(n):
            for v in range(n):
                distance_matrix[u][v] = min(distance_matrix[u][v], distance_matrix[u][k] + distance_matrix[k][v])

    
    return distance_matrix
        
if __name__ == '__main__':
    G = [
        [float('inf'), 5, 2, float('inf')], 
        [5, float('inf'), float('inf'), 3],  
        [2, float('inf'), float('inf'), float('inf')],  
        [float('inf'), 3, float('inf'), float('inf')]  
    ]
    print(floyd_warsh(G))