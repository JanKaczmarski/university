from dimacs import *
import heapq

class Node:
    def __init__(self):
        self.edges = {}
        self.merged = False

    # useful when merging vertices
    def addEdge(self, to, weight):
        self.edges[to] = self.edges.get(to, 0) + weight

    def delEdge(self, to):
        del self.edges[to]

def mergeVertices(G, x, y):
    # Adding all edges that go to y to x, deleting y vertex
    for v, weight in y.edges.items():
        x.addEdge(G[v], weight)
        G[v].addEdge(x, weight)
        G[v].delEdge(y)
    y.edges.clear()
    y.merged = True

def min_cut(graph, n):
    min_cut_value = float('inf')
    best_cut = None

    for _ in range(n - 1):
        # Priority queue for the most connected vertices
        pq = []
        added = [False] * n
        last = 0
        weights = [0] * n

        for i in range(n):
            if not graph[i].merged:
                heapq.heappush(pq, (-weights[i], i))

        s, t = -1, -1
        while pq:
            weight, node = heapq.heappop(pq)
            weight = -weight

            if added[node]:
                continue

            added[node] = True
            last = node

            for neighbor, w in graph[node].edges.items():
                if not added[neighbor] and not graph[neighbor].merged:
                    weights[neighbor] += w
                    heapq.heappush(pq, (-weights[neighbor], neighbor))

        s, t = last, max(range(n), key=lambda x: weights[x] if not graph[x].merged else -1)
        cut_value = weights[t]

        if cut_value < min_cut_value:
            min_cut_value = cut_value
            best_cut = (s, t)

        mergeVertices(graph, graph[s], graph[t])

    return best_cut, min_cut_value

def main():
    n, edges = loadWeightedGraph("graphs/simple")
    g = [Node() for _ in range(n)]

    for x, y, c in edges:
        g[x - 1].addEdge(y - 1, c)
        g[y - 1].addEdge(x - 1, c)

    res = min_cut(g, n)
    print("Minimum cut value:", res[1])
    print("Cut between nodes:", res[0])

if __name__ == '__main__':
    main()

