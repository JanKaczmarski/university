import random
import uuid
from math import ceil
from typing import List

import pycosat


def gene_form(n: int, k: int, a: float) -> List[int]:
    neg_value = [-1, 1]
    var_range = range(1, n + 1)

    res = []

    for _ in range(ceil(a * n)):
        clause = [random.choice(var_range) * random.choice(neg_value) for _ in range(k)]
        res.append(clause)

    return res


if __name__ == "__main__":
    n = 10
    k = 3
    T = 100
    f_name = f"data-{str(uuid.uuid1())[:5]}"
    for a_pred in range(1, 101):
        a = a_pred / 10
        solvable = 0
        for _ in range(T):
            formula = gene_form(n, k, a)
            is_sat = pycosat.solve(formula)
            if is_sat != "UNSAT" and is_sat != "UNKNOWN":
                solvable += 1

        with open(f_name, "a") as f:
            row = f"{a}\t{solvable / T}"
            print(row)
            f.write(row + "\n")
