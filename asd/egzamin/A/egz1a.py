# Jan Kaczmarski
# Algorytm wykorzystuje Floyda-Warshalla
# Na poczatku tworze macierz odleglosci dla trasy wykonanej pieszo
# porownuje mozliwe wyniki czyli 
# 1. na piechote
# 2. dla kazdego roweru z B wartosc to dist[s][i] + dist[i][t] * (p/q)
# przyrownuje wyniki i zwracam najmniejszy
# zlozonosc: O(V^3) 

from egz1atesty import runtests

def armstrong( B, G, s, t):
  n = 0
  # odczytuje ile jest wierzcholkow
  for edge in G:
    n = max(n, edge[0], edge[1])
  n += 1
  
  # towrze macierz nxn i uzupelniam jej wartosci poczotkawe o krawedzie
  # wystepujace w grafie G
  dist = [[float('inf') for _ in range(n)] for _ in range(n)]
  for u, v, w in G:
    dist[u][v] = w
    dist[v][u] = w
    dist[v][v] = dist[u][u] = 0
  
  # algorytm floyda warshalla
  for k in range(n):
    for u in range(n):
      for v in range(n):
        dist[u][v] = min(dist[u][v], dist[u][k] + dist[k][v])
  
  # ustawiamy wynik na przejscie calej trasy pieszo
  result = dist[s][t]
  # i sprawdzamy wyniki kiedy wsiadamy na rower
  for i, p, q in B:
    result = min(result, dist[s][i] + dist[i][t] * (p/q))
  
  # zwracam podloge z otrzymanego czasu
  return int(result)
  

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( armstrong, all_tests = False )