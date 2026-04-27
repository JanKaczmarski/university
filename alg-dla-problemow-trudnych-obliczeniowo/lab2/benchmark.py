import time

from dimacs import edgeList, isVC, loadGraph, saveSolution
from sol1 import sol1
from sol2 import sol2

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
    "k330_d",
    "k330_e",
    "k330_f",
    "f30",
    "f35",
    "f40",
    "f56",
    "m20",
    "m30",
    "m40",
    "m50",
    "m100",
    "p20",
    "p35",
    "p60",
    "p150",
    "p200",
    "r30_01",
    "r30_05",
    "r50_001",
    "r50_01",
    "r50_05",
    "r100_005",
    "r100_01",
    "r200_001",
    "r200_005",
]

SOLUTIONS = [
    sol1,
    sol2,
]


def run_benchmark(fn):
    name = fn.__name__
    print(f"\n=== {name} ===")
    print(f"{'Graph':<14} {'V':>5} {'E':>7}  {'|VC|':>6}  {'Valid':>5}  {'Time':>8}")
    print("-" * 55)

    for graph_name in GRAPHS:
        graph_path = f"graph/{graph_name}"
        try:
            G = loadGraph(graph_path)
        except IOError:
            print(f"{graph_name:<14}  --- (no file)")
            continue

        E = edgeList(G)
        V = len(G) - 1

        start = time.time()
        result = fn(E, set())
        elapsed = time.time() - start

        if result is None:
            print(f"{graph_name:<14} {V:>5} {len(E):>7}  {'None':>6}  {'?':>5}  {elapsed:.3f}s")
            continue

        valid = isVC(E, result)
        if valid:
            saveSolution(f"{graph_path}.sol", result)

        status = "OK" if valid else "FAIL"
        print(f"{graph_name:<14} {V:>5} {len(E):>7}  {len(result):>6}  {status:>5}  {elapsed:.3f}s")


for fn in SOLUTIONS:
    run_benchmark(fn)
