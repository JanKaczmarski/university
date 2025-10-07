from data import runtests

"""
Pomysl:
1a. Najpierw trzeba znalezc krolewski trakt, jest to MST

1. dla kazdego lorda wyznaczamy z jakim lordem sie przecina i zachowujemy to w tablicy, ala sąsiedztwa
2. Rekurencyjne podejscie, ala DFS: bierzemy lorda i dla wszystkich mozliwych sasiadow patrzymy rekurencyjnie max sciezke przy tym ustawieniu
3. Wyniki dla danego lorda zapisujemy w tablicy
4. Odczytujemy wynik jako max z tablicy wynikow

1. Szukanie: jako ze krolweski trakt to drzewo, szukanie wszystkich fortow jednego lorda mozna zrobic za pomoca BFS
Notatki:
Krolewski trakt to MST - minimalne drzewo rozpinajace
"""

# NOTE: MST works - was tested
def find(x, par):
  if x != par[x]:
    par[x] = find(par[x], par)
  return par[x]

def union(x, y, par, rank):
  x = find(x, par)
  y = find(y, par)
  if rank[x] > rank[y]:
    par[y] = x
  else:
    par[x] = y
    if rank[x] == rank[y]:
      rank[y] += 1

def mst(edges: list, n: int):
  # edges - sorted array of edges, (u, v, w) - edge from u to v with weight w
  parent = [i for i in range(n)]
  rank = [0 for _ in range(n)]
  graph = [[] for _ in range(n)]
  for u, v, w in edges:
    if find(u, parent) != find(v, parent):
      graph[u].append((v, w))
      graph[v].append((u, w))
      union(u, v, parent, rank)

  return graph

# NOTE:  span and lord_value works and was tested on tests: 1 to 5. No more because I didn't want to track it. But they should be good too
def dfs(u, graph, visited, lords, lord_id, span):
  visited[u] = True
  # if this is a property of lord
  if u in lords[lord_id]:
    span[u].add(lord_id)
    return True, 0
  # travers graph furhter
  sol = (False, 0)
  for v, w in graph[u]:
    if not visited[v]:
      valid_path, acc = dfs(v, graph, visited, lords, lord_id, span)
      if valid_path:
        span[u].add(lord_id)
        # We want to gather all scenarios, when from 1 vertex we get 2 places that belong to lord_id, then this allows us to store
        # all acumulated paths values
        sol = (True, sol[1] + acc + w)

  return sol


def get_span(graph: list, lords: list):
  n = len(graph)
  # span[i] - which lords have access to i-th place
  span = [set() for _ in range(n)]
  lord_val = [0 for _ in range(len(lords))]
  for i in range(len(lords)):
    # TODO: jk: change this False to visitedToken
    visited = [False for _ in range(n)]
    start_point = lords[i][0]
    visited[start_point] = True
    span[start_point].add(i)
    
    acc = 0
    for v, w in graph[start_point]:
      valid, dfsAcc = dfs(v, graph, visited, lords, i, span)
      if valid:
        acc += dfsAcc + w
    lord_val[i] = acc

  return span, lord_val


def get_conflict_graph(span: list[set], lords: list):
    # Initialize the conflict graph as a list of empty sets for each lord
    conflict_graph = [set() for _ in range(len(lords))]

    # Iterate through each set of lords sharing a palace
    for palace_lords in span:
        palace_lords_list = list(palace_lords)
        # Add conflicts for every pair of lords in the set
        for i in range(len(palace_lords_list)):
            for j in range(i + 1, len(palace_lords_list)):
                lord_i = palace_lords_list[i]
                lord_j = palace_lords_list[j]
                conflict_graph[lord_i].add(lord_j)
                conflict_graph[lord_j].add(lord_i)

    return conflict_graph


def max_weight_independent_set_backtracking(conflict_graph, lords_val):
    n = len(lords_val)
    max_value = 0
    best_subset = set()

    def backtrack(current_subset: set, current_value, index):
        nonlocal max_value, best_subset

        # Update the best result if the current subset is valid and better
        if current_value > max_value:
            max_value = current_value
            best_subset = current_subset

        # Try adding each lord starting from the current index
        for i in range(index, n):
            # Check if `i` conflicts with the current subset
            if all(neighbour not in current_subset for neighbour in conflict_graph[i]):
                # Add lord `i` to the subset
                current_subset.add(i)
                # Recurse with updated subset and value
                backtrack(current_subset, current_value + lords_val[i], i + 1)
                # Backtrack (remove the last added lord)
                current_subset.remove(i)

    # Start backtracking
    backtrack(set(), 0, 0)

    return max_value, best_subset


def my_solve(N, streets: list, lords:list):
  streets.sort(key=lambda x:x[2])
  print(f"Place: {N}, ulice: {len(streets)}, lordowie: {len(lords)}")
  graph = mst(streets, N + 1)
  span, lords_val = get_span(graph, lords)
  conflict_graph = get_conflict_graph(span, lords)
  result_val, result_set = max_weight_independent_set_backtracking(conflict_graph, lords_val)

  return result_val

runtests(my_solve)

