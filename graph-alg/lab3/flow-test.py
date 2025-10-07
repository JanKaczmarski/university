from dimacs import loadDirectedWeightedGraph
from os import listdir
from os.path import isfile, join
import time

def create_graph(n, edges):
    g = [[[0, 0, None] for _ in range(n)] for _ in range(n)]  # Ensure consistent structure
    neigh = [[] for _ in range(n)]
    for u, v, w in edges:
        g[u][v] = [w, 0, "f"]  # Forward edge
        g[v][u] = [0, 0, "b"]  # Reverse edge with capacity 0 initially
        neigh[u].append(v)
        neigh[v].append(u)
    return g, neigh

def dfs_visit(u, flow, visited, visitedToken, g, neigh, t):
    if u == t:
        return flow
    visited[u] = visitedToken
    for v in neigh[u]:
        if visited[v] != visitedToken:
            if g[u][v][2] == 'f' and g[u][v][0] - g[u][v][1] > 0:  # Forward edge
                bottleneck = dfs_visit(v, min(flow, g[u][v][0] - g[u][v][1]), visited, visitedToken, g, neigh, t)
                if bottleneck > 0:
                    g[u][v][1] += bottleneck
                    g[v][u][1] -= bottleneck
                    return bottleneck

            # Backward edge
            elif g[u][v][2] == 'b' and g[u][v][1] < 0:  # Negative flow means residual capacity
                bottleneck = dfs_visit(v, min(flow, -g[u][v][1]), visited, visitedToken, g, neigh, t)
                if bottleneck > 0:
                    g[u][v][1] += bottleneck  # Increase flow on backward edge
                    g[v][u][1] -= bottleneck  # Decrease flow on forward edge
                    return bottleneck
    return 0

def main():
    # NOTE: tests brake when there is a huge dfs run. After 10 seconds and when dfs_visit finish test will timeout
    # tests like these are for example grid ones
    tests = [join("graphs", f) for f in listdir("graphs") if isfile(join("graphs", f))]
    for test in tests:
        n, edges = loadDirectedWeightedGraph(test)
        # that's the way we get graph, we have 6 vetexes from 1...6 inclusive
        n = n + 1
        g, neigh = create_graph(n, edges) 

        s, t = 1, n - 1
        visited = [0 for _ in range(n)]
        visitedToken = 1
        max_flow = 0

        total_time = 0
        while True:
            if total_time > 10:
                print("Tests: %s timeout".format(test))
                break
            start = time.time()
            flow = dfs_visit(s, float('inf'), visited, visitedToken, g, neigh, t)
            if flow == 0:
                break

            max_flow += flow
            visitedToken += 1
            end = time.time()
            total_time += (end - start)

        print("************")
        print("Test Case: ", test)
        print("max_flow: ", max_flow)
        with open(test) as f:
            wanted = int(f.readline().split("=")[-1])
        print("correct value: ", wanted)
        print("test passed: ", wanted == max_flow)

if __name__ == '__main__':
  # TODO: this s,t should be passed to algorithm
  print(main())

