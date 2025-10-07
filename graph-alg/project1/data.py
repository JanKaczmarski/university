from testy import *

def read_problem(path):
  """
  <number of vertices>
  <number of edges>
  <number of lords>
  <solution>
  ... edge data ...
  <number of forts of lord 1>
  ... fort ids ...
  <number of forts of lord 2>
  ... fort ids ...
  ... etc ...
  """
  with open(path, "r") as f:

    def next_line():
      return f.readline().strip()

    V = int(next_line())
    E = int(next_line())
    L = int(next_line())
    solution = int(next_line())
    edges = [tuple(map(int, next_line().split())) for _ in range(E)]

    lords = []
    for _ in range(L):
      count = int(next_line())
      lords.append([int(next_line()) for _ in range(count)])

    return {"arg": [V, edges, lords], "hint": solution}


problems = [

{"arg": [4, [
  (1, 2, 5),
  (2, 3, 4),
  (3, 4, 6),
  ],
  [
    [1, 2],
    [3, 4],
  ]],
  "hint": 11},

# Obroncy: 0 2
{"arg": [6, [
    (1, 2, 4),
    (2, 3, 5),
    (3, 4, 6),
    (4, 5, 8),
    (5, 6, 7),
    (1, 6, 9),
    (2, 5, 10),
  ],
  [
    [1, 3],
    [2, 5],
    [4, 6],
  ]],
  "hint": 24},

# obroncy: 1
{"arg": [4, [
  (1, 2, 2),
  (2, 3, 3),
  (3, 4, 4),
  ],
  [
    [1, 3],
    [2, 4],
  ]],
  "hint": 7},

# obroncy: 0
{"arg": [3, [
  (1, 2, 3),
  (2, 3, 5),
  ],
  [
    [1, 3],
  ]],
  "hint": 8},

# obroncy: 0
{"arg": [4, [
  (1, 2, 2),
  (2, 3, 3),
  (2, 4, 5),
  ],
  [
    [1, 3, 4],
  ]],
  "hint": 10},

# obroncy: 1
{"arg": [5, [
  (1, 2, 3),
  (2, 3, 5),
  (2, 4, 4),
  (2, 5, 6),
  (1, 4, 8),
  (4, 3, 9),
  (5, 3, 11),
  (1, 5, 13),
  ],
  [
    [1, 3],
    [4, 5],
  ]],
  "hint": 10},

# Obroncy: 1
{"arg": [6, [
    (1, 2, 4),
    (2, 3, 5),
    (3, 4, 6),
    (4, 5, 8),
    (5, 6, 7),
    (1, 6, 9),
    (2, 5, 10),
  ],
  [
    [1, 4],
    [3, 6],
    [2, 5],
  ]],
  "hint": 21},

# obroncy: 0 3 5
{"arg": [12, [
    (1, 2, 21),
    (2, 3, 23),
    (3, 4, 22),
    (4, 5, 25),
    (3, 5, 29),
    (5, 7, 26),
    (7, 8, 22),
    (8, 9, 18),
    (4, 6, 24),
    (3, 6, 27),
    (6, 10, 19),
    (10, 11, 20),
    (11, 12, 21),
    (5, 6, 29),
    (7, 10, 30),
    (8, 11, 31),
    (9, 12, 32),
  ],
  [
    [1, 3],
    [2, 4],
    [5, 11],
    [6, 8],
    [7, 9],
    [10, 12],
  ]],
  "hint": 182},

{"arg": [17, [
    (1, 2, 8),
    (1, 3, 9),
    (1, 4, 10),
    (2, 3, 5),
    (2, 5, 6),
    (2, 7, 9),
    (3, 4, 7),
    (3, 5, 7),
    (3, 6, 8),
    (4, 6, 9),
    (5, 6, 7),
    (5, 7, 8),
    (5, 8, 9),
    (5, 9, 10),
    (6, 9, 11),
    (7, 8, 5),
    (7, 16, 9),
    (8, 9, 5),
    (8, 10, 6),
    (8, 11, 7),
    (8, 16, 9),
    (9, 11, 8),
    (9, 12, 8),
    (9, 17, 10),
    (10, 11, 5),
    (10, 12, 7),
    (10, 13, 8),
    (10, 17, 9),
    (11, 14, 9),
    (11, 16, 9),
    (12, 17, 10),
    (13, 14, 5),
    (13, 17, 4),
    (14, 15, 6),
    (14, 17, 6),
    (15, 16, 7),
  ],
  [
    [1, 4, 5],
    [3, 8],
    [6, 9, 10],
    [11, 12, 15],
    [7, 14, 17],
    [13, 16],
  ]],
  "hint": 57},

{"arg": [4, [
    (1, 2, 5),
    (2, 3, 6),
    (3, 4, 7),
    (4, 1, 8),
  ],
  [
    [1],
    [2],
    [3],
    [4],
  ]],
  "hint": 0},
{"arg": [10, [
    (1, 2, 1),
    (2, 3, 1),
    (3, 4, 1),
    (4, 5, 1),
    (5, 6, 1),
    (6, 7, 1),
    (7, 8, 1),
    (8, 9, 1),
    (9, 10, 1),
  ],
  [
    [1, 10],
    [2, 9],
    [3, 8],
    [4, 7],
    [5, 6],
  ]],
  "hint": 9},
]


graph_files = [
  "grid-small",
  "grid-med",
  "grid-large",
  "clique-small",
  "clique-med",
  "clique-large",
  "grid-3d-small",
  "grid-3d-large",
  "dense-line-small",
  "dense-line-med",
  "dense-line-large",
  "sparse-line-small",
  "sparse-line-med",
  "sparse-line-large",
  "random-sparse-small",
  "random-sparse-large",
]

problems += [read_problem(f"graphs/{name}") for name in graph_files]

def printarg(N, roads, lords):
    print(f"{N} miejsc, {len(roads)} drog, {len(lords)} lordow")

def printhint(hint):
    print("Wynik: {}".format(hint))

def printsol(sol):
    print("Uzyskany wynik: {}".format(sol))

def check(N, roads, lords, hint, sol):
    if hint == sol:
        print("Test zaliczony")
        return True
    else:
        print("NIEZALICZONY!")
        return False

def runtests(f):
    internal_runtests(printarg, printhint, printsol, check, problems, f)
