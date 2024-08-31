from egzP8btesty import runtests


def robot( G, P ):
    """Rozwiazanie: 
    Floyd-warshall -> po kazdym z punktow z tablicy P dochodzimy do celu
    """
    n = len(G)
    distancem = [[0 if v == u else float('inf') for v in range(n)] for u in range(n)]
    for u in range(n):
        for v, weight in G[u]:
            distancem[u][v] = distancem[v][u] = weight
    
    for k in range(n):
        for u in range(n):
            for v in range(n):
                distancem[u][v] = min(distancem[u][v], distancem[u][k] + distancem[k][v])
    sol = 0
    for i in range(len(P) - 1):
        sol += distancem[P[i]][P[i + 1]]

    return sol
    
runtests(robot, all_tests = True)
# G = [ [(1, 3), (2, 3)], [(0, 3), (4, 4)], [(0, 3), (3, 1), (4, 4)], [(2, 1), (4, 2)], [(1, 4), (2, 4), (3, 2)] ]
# P = [0, 3, 4]
# print(robot(G, P))