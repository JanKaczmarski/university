import dimacs
from os import listdir
from os.path import isfile, join
import time

def find(x, par):
  if x != par[x]:
    par[x] = find(par[x], par)
  return par[x]

def union(x, y, par, rank):
  x = find(x, par)
  y = find(y, par)
  if x == y:
    return
  if rank[x] > rank[y]:
    par[y] = x
  else:
    par[x] = y
    if rank[x] == rank[y]:
      rank[y] += 1

def widest_path(edges, n, s, t):
  edges.sort(key=lambda x: x[2] ,reverse=True)
  rank = [0 for _ in range(n)]
  par = [i for i in range(n)]
  for u, v, w in edges:
    union(u, v, par, rank)
    if find(s, par) == find(t,par):
      return w

  return -1


def main():
  # vertexes are numbererd from 1 to V inclusive
  tests = [join("graphs", f) for f in listdir("graphs") if isfile(join("graphs", f))]
  for test in tests:
    V, L = dimacs.loadWeightedGraph(test)
    out = widest_path(L, V + 1, 1, V)
    print(f"***Test {test}***") 
    print("Widest Path: ", out)
    print("Solution: ", dimacs.readSolution(test))
    print("Test passed: ", int(out) == int(dimacs.readSolution(test)))


if __name__ == '__main__':
  main()

