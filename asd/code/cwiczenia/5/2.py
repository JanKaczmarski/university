"""
TRESC
Uniwersalne ujscie:
Graf zorientowany, uniwersalne ujscie to taki wierzcholek
ze wszystkie inne wierzcholki do niego wchodza a zadna krawedz z niego nie wychodzi
"""
# Graf jest podany macierzowo

# O(n^2)
def zad2_s(G):
    n = len(G)
    for i in range(n):
        flag = True
        for j in range(n):
            if (G[i][j] != 0) or (G[j][i] != 1 and i != j):
                flag = False
                continue
            
        if flag: return i
        
    return False
    
"""
O(n)
1.  Jesli 0 to w prawo
    Jesli 1 to w dol
2. Jesli doszlismy do sciany to rozpatrujemy aktualne i - to jest nasz kandydat
"""

