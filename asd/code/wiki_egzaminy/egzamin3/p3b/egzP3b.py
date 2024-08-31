from egzP3btesty import runtests 
from queue import PriorityQueue

class Node:
    def __init__(self, value):
        self.val = value
        self.parent = self
        self.rank = 0


def get_edges(G):
    result = []
    for u in range(len(G)):
        for v, w in G[u]:
            if v > u:
                result.append((u, v, w))
    return result


def lufthansa ( G ):
    """
    Algorytm kruskala tworzacy najwieksze drzewo rozpinające 
    """
    n = len(G)
    edges = sorted(get_edges(G), key=lambda x: x[2], reverse=True)
    par = [i for i in range(n)]
    rank = [0 for _ in range(n)]
    def findset(x):
        if x != par[x]:
            par[x] = findset(par[x])
        return par[x]

    def unionset(x, y):
        x = findset(x)
        y = findset(y)
        
        if rank[x] > rank[y]:
            par[y] = x
        else:
            par[x] = y
            if rank[y] == rank[x]:
                rank[y] += 1
    
    deleted_sum = 0
    flag = True
    for u, v, w in edges:
        if findset(u) != findset(v):
            unionset(u, v)
        else:
            if flag:
                flag = False
                continue
            deleted_sum += w

    return deleted_sum

runtests ( lufthansa, all_tests=True )