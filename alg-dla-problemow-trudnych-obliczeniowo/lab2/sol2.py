from collections import defaultdict
from typing import List, Set, Tuple

from dimacs import edgeList, loadGraph


def sol2(edges: List[Tuple[int, int]], res: Set[int]):
    vertexes = defaultdict(int)
    for u, v in edges:
        vertexes[u] += 1
        vertexes[v] += 1

    vertexes_list = sorted(list(vertexes.items()), key=lambda x: x[1], reverse=True)

    for u, _ in vertexes_list:
        res.add(u)
        if is_vc(edges, res):
            return res

    return res


def is_vc(edges: list[Tuple[int, int]], vertexes: Set[int]):
    for u, v in edges:
        if u not in vertexes or v not in vertexes:
            return False

    return True


if __name__ == "__main__":
    g = loadGraph("graph/e5")
    edges = edgeList(g)

    print(sol2(edges, set()))


"""
algorytm O(logn)-aproksymacyjny (dodajemy do rozwiązania wierzchołek o najwyższym aktualnym stopniu)
"""
