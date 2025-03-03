"""
Jan Kaczmarski
Modyfikacja algorytmu djikstry sprawdzająca warunki podane
w zadaniu przed zmienieniem distance
zlozonosc czasowa O(mlogn)
"""

from egz2btesty import runtests
from collections import deque
from queue import PriorityQueue

def convert(edges):
  G = []
  for u, v, w, kind in edges:
    # extend list
    while len(G) <= u:
        G.append([])
    while len(G) <= v:
        G.append([])
    
    # add edge to graph
    if kind == 'I': kind = 5
    else: kind = 10
    G[u].append([v, w, kind])
    G[v].append([u, w, kind])

  return G


def tory_amos(E, A, B):
  G = convert(E)
  n = len(G)
  q = PriorityQueue()
  # P, I
  distance = [float('inf') for _ in range(n)]
  distance[A] = 0
  # dist_to_A, vertex, kind
  q.put((0, A, 0))
  while not q.empty():
    _, u, p_kind = q.get()
    for v, w, kind in G[u]:
      cost = 0
      if p_kind == 0:
        cost += 0
      elif p_kind != kind:
        cost += 20
      else:
        cost += kind
      if distance[v] > distance[u] + cost + w:
        distance[v] = distance[u] + cost + w
        q.put((distance[v], v, kind))
  
  return distance[B]      
  
runtests( tory_amos, all_tests = True )