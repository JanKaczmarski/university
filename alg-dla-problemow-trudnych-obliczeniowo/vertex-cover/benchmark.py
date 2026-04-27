import threading
import time
from typing import Callable, Dict, List, Optional

from brute_force import brute_vertex_cover
from dimacs import edgeList, isVC, loadGraph, saveSolution
from sol2 import solve_vc_2
from sol3 import solve_vc_3

TIMEOUT = 20  # seconds per algorithm per graph

GRAPHS = [
    "e5",
    "e10",
    "e20",
    "e40",
    "e150",
    "s25",
    "s50",
    "s500",
    "b20",
    "b30",
    "b100",
    "k330_a",
    "k330_b",
    "k330_c",
    "m20",
    "m30",
    "m40",
    "m50",
    "m100",
    "p20",
    "p35",
    "p60",
    "p150",
    "r30_01",
    "r30_05",
    "r50_001",
    "r50_01",
    "r50_05",
    "r100_005",
]

# Each entry: (label, callable(graph_path) -> Optional[List[int]])
ALGORITHMS = [("brute_force", brute_vertex_cover), ("exp_two", solve_vc_2), ("vc3", solve_vc_3)]


def run_with_timeout(fn: Callable, graph_path: str):
    """Run fn(graph_path) in a daemon thread.
    Returns (result, elapsed_seconds); result is None on timeout."""
    result_box: List[Optional[List[int]]] = [None]

    def target():
        result_box[0] = fn(graph_path)

    t = threading.Thread(target=target, daemon=True)
    start = time.time()
    t.start()
    t.join(TIMEOUT)
    elapsed = time.time() - start
    return (result_box[0] if not t.is_alive() else None), elapsed


def benchmark_graph(name: str) -> Dict[str, str]:
    graph_path = f"graph/{name}"
    G = loadGraph(graph_path)
    E = edgeList(G)
    V = len(G) - 1  # vertex 0 is a dummy placeholder

    print(f"{name:<12} V={V:<5} E={len(E):<7}")

    best: Optional[List[int]] = None
    statuses = {}

    for label, fn in ALGORITHMS:
        result, elapsed = run_with_timeout(fn, graph_path)

        if result is None:
            print(f"  [{label}]  TIMEOUT ({elapsed:.1f}s)")
            statuses[label] = "timeout"
            continue

        if not isVC(E, result):
            print(f"  [{label}]  INVALID ({elapsed:.2f}s)")
            statuses[label] = "invalid"
            continue

        print(f"  [{label}]  |VC|={len(result)}  ({elapsed:.2f}s)")
        statuses[label] = "success"

        if best is None or len(result) < len(best):
            best = result

    if best is not None:
        sol_path = f"graph/{name}.sol"
        saveSolution(sol_path, best)
        print(f"  => saved best |VC|={len(best)}")
    else:
        print(f"  => no solution found")

    return statuses


if __name__ == "__main__":
    print("=" * 50)
    print(f"Benchmark: each algorithm runs independently")
    print(f"per graph with a {TIMEOUT}s timeout.")
    print("=" * 50)
    print()

    # Inicjalizacja statystyk dla każdego algorytmu
    stats = {label: {"success": 0, "timeout": 0, "invalid": 0} for label, _ in ALGORITHMS}

    for name in GRAPHS:
        graph_statuses = benchmark_graph(name)
        # Zliczanie wyników dla obecnego grafu
        for label, status in graph_statuses.items():
            stats[label][status] += 1
        print()

    # Wyświetlenie podsumowania
    print("=" * 50)
    print("FINAL SUMMARY")
    print("=" * 50)
    print(f"{'Algorithm':<15} | {'Success':<10} | {'Timeout':<10} | {'Invalid':<10}")
    print("-" * 55)
    for label, _ in ALGORITHMS:
        s = stats[label]["success"]
        t = stats[label]["timeout"]
        i = stats[label]["invalid"]
        print(f"{label:<15} | {s:<10} | {t:<10} | {i:<10}")
