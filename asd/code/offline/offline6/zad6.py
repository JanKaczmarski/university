from zad6testy import runtests
from queue import PriorityQueue

def jump(G, Q, v, u):
    n = len(G)
    # jump from v to u-adjacent vertexes
    u_adjacent = G[u]
    # cost from v to u
    w1 = G[v][u]
    for i in range(n):
        if G[u][i] == 0 or i == v:
            continue
        w2 = G[u][i]
        Q.put((max(w1, w2), i, False))

def jumper( G, s, w ):
    n = len(G)
    distance = [float('inf') for _ in range(n)]
    parent = [None for _ in range(n)]
    Q = PriorityQueue()
    distance[s] = 0
    # weight, vertex, can_jump 
    Q.put((0, s, True))
    
    while Q.not_empty:
        weight, vertex, flag = Q.get()
        
        for i in range(n):
            if G[vertex][i] == 0 or parent[i] == vertex:
                continue
            if distance[i] > distance[vertex] + weight and flag:
                distance[i] = distance[vertex] + weight
                Q.put((G[vertex][i], i, True))
            if not flag:
                Q.put((G[vertex][i], i, True))
            
            if flag:
                jump(G, Q, vertex, i)
            
            if vertex == w:
                return distance[w]
    
    return distance[w]

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( jumper, all_tests = True )
#G = [[0, 1, 0, 0], [1, 0, 2, 0], [0, 2, 0, 3], [0, 0, 3, 0]]
#s = 0
#w = 3
#print(jumper(G, s, w))