import threading
import time
from itertools import combinations

from dimacs import *

TIMEOUT = 20  # seconds

GRAPHS = [
    "e5", "e10", "e20", "e40", "e150",
    "s25", "s50", "s500",
    "b20", "b30", "b100",
    "k330_a", "k330_b", "k330_c",
    "m20", "m30", "m40", "m50", "m100",
    "p20", "p35", "p60", "p150",
    "r30_01", "r30_05",
    "r50_001", "r50_01", "r50_05",
    "r100_005",
]


def brute_vertex_cover(graph_name: str):
    graph = loadGraph(graph_name)
    v = len(graph)
    for k in range(v):
        combs = combinations(range(v), k)
        for comb in combs:
            # check if valid vertex-cover
            valid = True
            for origin in range(1, v):
                for dest in graph[origin]:
                    if not (origin in comb or dest in comb):
                        valid = False
                        break
            if valid:
                return comb


def solve_with_timeout(graph_name: str):
    """Run brute_vertex_cover in a daemon thread; return result or None on timeout."""
    result_box = [None]

    def target():
        result_box[0] = brute_vertex_cover(graph_name)

    t = threading.Thread(target=target, daemon=True)
    t.start()
    t.join(TIMEOUT)
    return result_box[0] if not t.is_alive() else None


if __name__ == "__main__":
    print(f"{'Graph':<12} {'V':>5} {'E':>7}  Result")
    print("-" * 40)

    for name in GRAPHS:
        graph_path = f"graph/{name}"
        sol_path = f"graph/{name}.sol"

        G = loadGraph(graph_path)
        E = edgeList(G)
        V = len(G) - 1  # vertex 0 is a dummy placeholder

        start = time.time()
        result = solve_with_timeout(graph_path)
        elapsed = time.time() - start

        if result is None:
            status = f"TIMEOUT ({elapsed:.1f}s)"
        elif not isVC(E, set(result)):
            status = f"INVALID ({elapsed:.2f}s)"
        else:
            saveSolution(sol_path, set(result))
            status = f"|VC|={len(result)}  ({elapsed:.2f}s)  -> saved"

        print(f"{name:<12} {V:>5} {len(E):>7}  {status}")
