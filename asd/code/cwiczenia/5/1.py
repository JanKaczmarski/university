"""
Sprawdzanie czy graf jest dwudzielny
Graf jest w postaci macierzowej
"""
from collections import deque

# Proste jak budowa cepa
def zad1(G, n):
    q = deque()
    color = [0 for _ in range(n)]
    q.append(0)
    # colors are 1 and -1
    color[0] = 1
    while len(list(q)) > 0:
        v = q.popleft()
        for u in range(n):
            if u == v:
                continue
            if color[u] == color[v]:
                return False
            elif color[u] == 0:
                color[u] == -color[v]
        q.append(u)
        
    return True  
            
        
        
