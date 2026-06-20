"""
POC: Heurystyczne sortowanie kandydatów na lustro w algorytmie DFS.

Dla każdej pozycji kandydata na liście (pola wzdluz sciezki lasera)
sprawdzamy ile kotow zostanie oswietlonych jesli tam postawimy lustro (/ lub backslash).
Nastepnie sortujemy kandydatow malejaco - najpierw probujemy najbardziej obiecujace.
"""

from typing import NamedTuple


# ---------------------------------------------------------------------------
# Typy
# ---------------------------------------------------------------------------


class PathStep(NamedTuple):
    x: int
    y: int
    dir: str  # 'N', 'S', 'W', 'E'


# ---------------------------------------------------------------------------
# Pomocnicze funkcje geometryczne
# ---------------------------------------------------------------------------

DELTAS = {"N": (0, -1), "S": (0, 1), "W": (-1, 0), "E": (1, 0)}

REFLECT = {
    "/": {"N": "E", "S": "W", "E": "N", "W": "S"},
    "\\": {"N": "W", "S": "E", "E": "S", "W": "N"},
}


def reflect(mirror: str, direction: str) -> str:
    return REFLECT[mirror][direction]


# ---------------------------------------------------------------------------
# Symulacja lasera
# ---------------------------------------------------------------------------


def simulate_laser(grid: list[list[str]], W: int, H: int, start_x: int, start_y: int, start_dir: str) -> int:
    """
    Symuluje laser od (start_x, start_y) w kierunku start_dir.
    Zwraca liczbę unikalnych kotów ('O') oświetlonych przez promień.
    """
    x, y, d = start_x, start_y, start_dir
    illuminated: set[tuple[int, int]] = set()
    visited: set[tuple[int, int, str]] = set()

    while 0 <= x < W and 0 <= y < H:
        state = (x, y, d)
        if state in visited:
            break
        visited.add(state)

        cell = grid[y][x]

        if cell == "#":
            break

        if cell == "O":
            illuminated.add((x, y))

        if cell in ("/", "\\"):
            d = reflect(cell, d)

        dx, dy = DELTAS[d]
        x += dx
        y += dy

    return len(illuminated)


# ---------------------------------------------------------------------------
# Główna funkcja heurystyki
# ---------------------------------------------------------------------------


class CandidateScore(NamedTuple):
    cat_count: int  # ile kotów oświetla to ustawienie (wyżej = lepiej)
    x: int
    y: int
    mirror_type: str  # '/' lub '\\'


def score_candidates(
    grid: list[list[str]],
    W: int,
    H: int,
    start_x: int,
    start_y: int,
    start_dir: str,
    candidates: list[PathStep],
) -> list[CandidateScore]:
    """
    Dla każdej pary (pozycja kandydata, typ lustra) ocenia ile kotów zostanie
    oświetlonych gdyby tam postawić lustro, a następnie zwraca listę
    posortowaną malejąco według liczby kotów.

    Parametry
    ----------
    grid        : plansza (lista list znaków, modyfikowana tymczasowo)
    W, H        : wymiary planszy
    start_x/y   : pozycja lasera (startowa dla pełnej symulacji)
    start_dir   : kierunek lasera
    candidates  : lista PathStep – pola ścieżki lasera, na których można postawić lustro

    Zwraca
    -------
    Lista CandidateScore posortowana malejąco po cat_count.
    Kandydaci z tym samym wynikiem zachowują oryginalną kolejność (stable sort).
    """
    scores: list[CandidateScore] = []

    for step in candidates:
        x, y = step.x, step.y

        if grid[y][x] != ".":
            continue  # pole już zajęte

        for mirror in ("/", "\\"):
            # Sprawdzamy czy po odbiciu promień nie uderzy od razu w ścianę / wyjdzie poza planszę
            new_dir = reflect(mirror, step.dir)
            dx, dy = DELTAS[new_dir]
            nx, ny = x + dx, y + dy
            if not (0 <= nx < W and 0 <= ny < H):
                continue  # promień natychmiast wychodzi poza planszę
            if grid[ny][nx] == "#":
                continue  # promień natychmiast uderza w ścianę

            # Tymczasowo ustaw lustro i symuluj
            grid[y][x] = mirror
            cats = simulate_laser(grid, W, H, start_x, start_y, start_dir)
            grid[y][x] = "."

            scores.append(CandidateScore(cats, x, y, mirror))

    # Sortowanie malejące po liczbie kotów (stable – zachowuje kolejność przy remisie)
    scores.sort(key=lambda s: s.cat_count, reverse=True)
    return scores


# ---------------------------------------------------------------------------
# Testy
# ---------------------------------------------------------------------------


def parse_grid(raw: str) -> tuple[list[list[str]], int, int]:
    lines = [line for line in raw.strip().splitlines() if line]
    H = len(lines)
    W = len(lines[0])
    grid = [list(line) for line in lines]
    return grid, W, H


def find_laser(grid: list[list[str]], W: int, H: int) -> tuple[int, int, str]:
    laser_chars = {"A": "N", "V": "S", "<": "W", ">": "E"}
    for y in range(H):
        for x in range(W):
            if grid[y][x] in laser_chars:
                return x, y, laser_chars[grid[y][x]]
    raise ValueError("Brak lasera na planszy")


def collect_path(grid: list[list[str]], W: int, H: int, start_x: int, start_y: int, start_dir: str) -> list[PathStep]:
    """Zbiera ścieżkę lasera – pola '.' które można rozważyć jako kandydatów."""
    x, y, d = start_x, start_y, start_dir
    path: list[PathStep] = []
    visited: set[tuple[int, int, str]] = set()

    while 0 <= x < W and 0 <= y < H:
        state = (x, y, d)
        if state in visited:
            break
        visited.add(state)

        cell = grid[y][x]
        if cell == "#":
            break
        if cell == ".":
            path.append(PathStep(x, y, d))
        if cell in ("/", "\\"):
            d = reflect(cell, d)

        dx, dy = DELTAS[d]
        x += dx
        y += dy

    return path


def run_test(name: str, raw: str) -> None:
    print(f"\n{'='*60}")
    print(f"TEST: {name}")
    print("=" * 60)

    grid, W, H = parse_grid(raw)
    lx, ly, ld = find_laser(grid, W, H)
    path = collect_path(grid, W, H, lx, ly, ld)

    print("Plansza:")
    for row in grid:
        print("  " + "".join(row))
    print(f"Laser: ({lx},{ly}) kierunek={ld}")
    print(f"Kandydaci na ścieżce: {len(path)} pól")

    ranked = score_candidates(grid, W, H, lx, ly, ld, path)

    print("\nTop 5 najlepszych ustawień lustro:")
    for i, s in enumerate(ranked[:5]):
        print(f"  {i+1}. ({s.x},{s.y}) lustro='{s.mirror_type}'  koty={s.cat_count}")

    if not ranked:
        print("  (brak kandydatów)")


# ---------------------------------------------------------------------------
# Przykłady testowe
# ---------------------------------------------------------------------------

TEST_1 = """
>....O
......
......
O.....
......
......
"""

TEST_2 = """
>........
.........
....O....
.........
O........
.........
......O..
"""

TEST_3 = """
>...#....
.........
....O....
.........
#........
.........
......O..
"""

TEST_4 = """
>.......
........
....O...
........
........
....O...
"""

if __name__ == "__main__":
    run_test("Prosty: laser poziomy, 2 koty", TEST_1)
    run_test("Większa plansza, 3 koty", TEST_2)
    run_test("Ze ścianami", TEST_3)
    run_test("2 koty w tej samej kolumnie", TEST_4)
