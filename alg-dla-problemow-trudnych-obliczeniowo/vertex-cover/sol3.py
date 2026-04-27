from typing import Dict, Iterable, List, Optional, Set

from dimacs import *


def vc_3(graph: Dict[int, Set[int]], sol: Set[int], k: int) -> Optional[Set[int]]:
    u = -1
    for node, neighbors in graph.items():
        if len(neighbors) > 0:
            u = node
            break

    if u == -1:
        return sol

    if k <= 0:
        return None

    # remove u
    sol.add(u)
    graph_minus_u = _remove_vertexes(graph, {u})

    local_res = vc_3(graph_minus_u, sol, k - 1)
    if local_res is not None:
        return local_res

    sol.remove(u)

    neigh = graph[u]
    neigh_num = len(neigh)

    if neigh_num > k:
        return None

    # remove neighbours
    sol.update(neigh)
    graph_minus_n = _remove_vertexes(graph, neigh)

    local_res = vc_3(graph_minus_n, sol, k - neigh_num)
    if local_res is not None:
        return local_res

    sol -= neigh

    return None


def _remove_vertexes(graph: Dict[int, Set[int]], vertexes: Set[int]) -> Dict[int, Set[int]]:
    sol = {}
    for key, value in graph.items():
        if key not in vertexes:
            sol[key] = value - vertexes

    return sol


def solve_vc_3(graph_name: str) -> Optional[List[int]]:
    raw_graph = loadGraph(graph_name)
    num_vertexes = len(raw_graph)

    graph: Dict[int, Set[int]] = {v: neighbors.copy() for v, neighbors in enumerate(raw_graph) if len(neighbors) > 0}

    vertexes: Set[int] = set()

    for k in range(1, num_vertexes):
        res = vc_3(graph, vertexes, k)
        if res is not None:
            return list(res)

    return None


if __name__ == "__main__":
    print("vc3 dry run")
    res = solve_vc_3("graph/e10")
    print(res)
