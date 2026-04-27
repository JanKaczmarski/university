from typing import List, Tuple

import pycosat

from dimacs import *


def index(i, j):
    # ensure no 0 variable
    return int((i + j) * (i + j + 1) / 2 + i) + 1


def gen_clause(edges: List[Tuple[int, int]], num_ver: int, cover_size: int):
    clauses = []

    # --- 1. ensure we have cover ---
    for u, v in edges:
        if u == 0 or v == 0:
            continue  # skip dummy vertex
        clauses.append([u + 1, v + 1])  # shift to SAT vars

    # --- 2. yi,0 and y0,j ---
    for i in range(1, num_ver):  # skip 0
        clauses.append([index(i, 0)])
        clauses.append([-index(0, i)])

    # --- 3. solution is <= cover_size ---
    clauses.append([-index(num_ver - 1, cover_size + 1)])

    # --- 4. DP transitions ---
    for i in range(1, num_ver):
        for j in range(1, num_ver):
            clauses.append([-index(i - 1, j), index(i, j)])
            clauses.append([-index(i - 1, j - 1), -(i + 1), index(i, j)])

    return clauses


def main():
    g = loadGraph("graph/s500")

    edges = edgeList(g)

    for k in range(1, len(g)):
        clauses = gen_clause(edges, len(g), k)

        # debug safety check
        for c in clauses:
            for lit in c:
                assert lit != 0

        print(f"k = {k}")
        res = pycosat.solve(clauses)
        if res != "UNSAT":
            print("found")
            return res
        else:
            print(res)


if __name__ == "__main__":
    main()
