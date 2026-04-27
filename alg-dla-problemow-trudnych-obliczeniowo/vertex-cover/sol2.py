from typing import List, Optional, Set, Tuple

from dimacs import *


def vc_2(edges: List[Tuple[int, int]], k: int, vertexes: Set[int]) -> Optional[Set[int]]:
    x, y = -1, -1
    for u, v in edges:
        if u in vertexes or v in vertexes:
            continue
        x, y = u, v

    # every edge is covered
    if x == -1 and y == -1:
        return vertexes

    # some edges are not covered and we can't add more vertexes to our set
    if k == 0:
        return None

    vertexes.add(x)
    s1 = vc_2(edges, k - 1, vertexes)
    if s1 is not None:
        return s1
    vertexes.remove(x)

    vertexes.add(y)
    s2 = vc_2(edges, k - 1, vertexes)
    if s2 is not None:
        return s2
    vertexes.remove(y)

    return None


def solve_vc_2(graph_name: str) -> Optional[List[int]]:
    graph_set = loadGraph(graph_name)
    num_vertexes = len(graph_set)
    graph_edges = edgeList(graph_set)
    vertexes: Set[int] = set()

    for k in range(1, num_vertexes):
        res = vc_2(graph_edges, k, vertexes)
        if res is not None:
            return list(res)

    return None


"""
def VC( G, k, S ):
  # G to graf wejściowy, k liczba wierzchołków, które możemy użyć
  # S to zbiór wierzchołków, który budujemy

  wybierz dowolną krawędź e = {u,v}, która nie jest
  jeszcze pokryta (czyli ani u ani v nie jest wybrany)

  if nie ma takiej krawędzi:
    return S  # rozwiązanie znalezione

  if k == 0:
    return None # nie ma rozwiązania

  S1 = VC( G - {u}, k-1, S + {u} )
  S2 = VC( G - {v}, k-1, S + {v} )

  if S1:
    return S1
  else:
    return S2

"""
