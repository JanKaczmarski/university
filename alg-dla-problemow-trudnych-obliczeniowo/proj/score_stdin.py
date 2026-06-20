"""
Wczytuje plansze ze stdin (format identyczny z zadaniem: W H L, potem H wierszy)
i dla każdego lasera wypisuje posortowane kandydaty na następne lustro.

Użycie:
    python3 score_stdin.py < t1.in
"""

import sys
from mirror_heuristic_poc import (
    PathStep,
    CandidateScore,
    score_candidates,
    simulate_laser,
    collect_path,
    DELTAS,
    reflect,
)

LASER_CHARS = {"A": "N", "V": "S", "<": "W", ">": "E"}


def find_lasers(grid: list[list[str]], W: int, H: int) -> list[tuple[int, int, str]]:
    lasers = []
    for y in range(H):
        for x in range(W):
            if grid[y][x] in LASER_CHARS:
                lasers.append((x, y, LASER_CHARS[grid[y][x]]))
    return lasers


def draw_grid_with_path(grid: list[list[str]], W: int, H: int, path: list[PathStep]) -> None:
    """Rysuje planszę zaznaczając ścieżkę lasera znakiem '*' (tylko pola '.')."""
    path_set = {(s.x, s.y) for s in path}
    for y in range(H):
        row = ""
        for x in range(W):
            ch = grid[y][x]
            if ch == "." and (x, y) in path_set:
                row += "*"
            else:
                row += ch
        print("  " + row)


def main() -> None:
    data = sys.stdin.read().split()
    idx = 0

    W = int(data[idx]); idx += 1
    H = int(data[idx]); idx += 1
    L = int(data[idx]); idx += 1

    grid: list[list[str]] = []
    for y in range(H):
        grid.append(list(data[idx])); idx += 1

    total_cats = sum(1 for row in grid for ch in row if ch == "O")
    lasers = find_lasers(grid, W, H)

    print(f"Plansza {W}x{H}, luster do rozstawienia: {L}, kotów: {total_cats}")
    print()

    for laser_idx, (lx, ly, ld) in enumerate(lasers):
        baseline = simulate_laser(grid, W, H, lx, ly, ld)
        path = collect_path(grid, W, H, lx, ly, ld)
        ranked = score_candidates(grid, W, H, lx, ly, ld, path)

        print(f"{'─'*60}")
        print(f"Laser #{laser_idx + 1}  pozycja=({lx},{ly})  kierunek={ld}")
        print(f"Aktualnie oświetlonych kotów: {baseline}/{total_cats}")
        print(f"Pól na ścieżce do rozważenia: {len(path)}")
        print()

        print("Plansza (ścieżka lasera oznaczona '*'):")
        draw_grid_with_path(grid, W, H, path)
        print()

        if not ranked:
            print("  Brak kandydatów (ścieżka pusta lub wszystkie odcinane).")
        else:
            # Pogrupuj według liczby kotów
            buckets: dict[int, list[CandidateScore]] = {}
            for s in ranked:
                buckets.setdefault(s.cat_count, []).append(s)

            print(f"{'#':>3}  {'(x,y)':>7}  {'lustro':>6}  {'koty':>4}  zmienionych")
            print(f"{'─'*3}  {'─'*7}  {'─'*6}  {'─'*4}  {'─'*10}")
            for rank, s in enumerate(ranked, 1):
                delta = s.cat_count - baseline
                delta_str = f"+{delta}" if delta > 0 else str(delta)
                print(f"{rank:>3}.  ({s.x:2},{s.y:2})   '{s.mirror_type}'    {s.cat_count:>4}  {delta_str}")

            improving = [s for s in ranked if s.cat_count > baseline]
            print()
            print(f"Kandydatów z poprawą: {len(improving)} / {len(ranked)}")
            if improving:
                best = ranked[0]
                print(f"Najlepszy: ({best.x},{best.y}) lustro='{best.mirror_type}' -> {best.cat_count} kotów")

        print()


if __name__ == "__main__":
    main()
