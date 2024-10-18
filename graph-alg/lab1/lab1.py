import dimacs

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

def widest_path(edges, n, s, t):
  edges.sort(reverse=True)
  rank = [0 for _ in range(n)]
  par = [i for i in range(n)]
  # drzewo do kiedy rank[s] != rank[t]
  i = 0
  while par[s] != par[t]:
    u, v, w = edges[i]
    if find(u, par) != find(v, par):
      union(u, v, par, rank)
    i += 1
  return w

def main():
  # L - edges are from 0 to V-1
  V, L = dimacs.loadWeightedGraph("graphs-lab1/path10")
  print("Input edges: {0}".format(L))
  print("Number of vertexes: {0}".format(V))
  print("Widest Path: {0}".format(widest_path(L, V, 0, V-1)))
  return None


if __name__ == '__main__':
  main()
