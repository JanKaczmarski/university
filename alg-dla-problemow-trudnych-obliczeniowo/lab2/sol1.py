from typing import List, Set, Tuple

from dimacs import *


def sol1(edges: List[Tuple[int, int]], res: Set[int]):
    for u, v in edges:
        if u not in res and v not in res:
            res.add(u)
            res.add(v)

    return list(res)


if __name__ == "__main__":
    g = loadGraph("graph/e5")
    edges = edgeList(g)

    print(sol1(edges, set()))

"""
algorytm 2-aproksymacyjny (wybieramy niepokrytą krawędź, dodajemy oba jej wierzchołki do rozwiązania)
"""
