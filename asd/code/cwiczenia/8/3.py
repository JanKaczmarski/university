"""
Floyd-Warshall
Algorytm obliczajacy tapniecie przechodnie grafu skierowanego
reprezentowanego w postaci macierzowej.
tapniecie przechodine grafu: 
Graf w ktorym kazdych 2 wierzcholkow u i v 
ma zaznaczona krawedz z u do v
wtw. gdy w grafie istnieje sciezka z u do v
"""

def sol(G):
    # matrix given graph
    n = len(G)
    matrix = [[G[i][j] for j in range(n)] for i in range(n)]
    for k in range(n):
        for i in range(n):
            for j in range(n):
                matrix[i][j] = matrix[i][j] or \
                                (matrix[i][k] and matrix[k][j])
                                
