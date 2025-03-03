# Jan Kaczmarski
# Rozwiązanie wykorzystuje algorytm BFS
# Dla kazdego wierzcholka z ktorego wychodzi Grzyb uruchamiamy BFS i zliczamy 
# odleglosci do kazdego innego wierzcholka O(k * (V + E)) k algorytmów BFS
# Potem porównujemy tablice distance obliczone dla kazdego grzyba 
# jesli do jakeigos wierzcholka grzyb `d` dostał się pierwszy to zwiększamy wynik o 1
# koszt wyznaczania wyniku to V * k -> sprawdzamy min dla kazdego wierzhcolka w k tablicach
# Zlozonosc Czasowa: O(k*(V + E) + V * k) = O(V + E)


from egz3atesty import runtests
from collections import deque

def bfs(G, s):
  n = len(G)
  distance = [float('inf') for _ in range(n)]
  visited = [False for _ in range(n)]
  distance[s] = 0
  visited[s] = True
  q = deque()
  q.append(s)
  while len(q) > 0:
    u = q.popleft()
    for v in G[u]:
      if not visited[v]:
        visited[v] = True
        distance[v] = distance[u] + 1
        q.append(v)
    
  return distance

def mykoryza( G,T,d ):
  n = len(G)
  k = len(T)
  sol = 0
  distances = []
  for i in range(k):
    distances.append(bfs(G, T[i]))
  
  for u in range(n):
    smallest = [float('inf'), []]
    for g in range(k):
      if distances[g][u] < smallest[0]:
        smallest = [distances[g][u], [g]]
      elif distances[g][u] == smallest[0]:
        smallest[1].append(g)
    if d == min(smallest[1]): sol += 1
  
  return sol

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( mykoryza, all_tests = True )