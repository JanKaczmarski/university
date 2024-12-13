from dimacs import *
from queue import PriorityQueue
from os import listdir
from os.path import isfile, join

class Node:
    def __init__(self):
        self.edges = {}
        self.merged = False
        self.mergedInto = []

    def addEdge(self, to, weight):
        self.edges[to] = self.edges.get(to, 0) + weight

    def delEdge(self, to):
        if to in self.edges:
            del self.edges[to]


def mergeVertices(G, x, y):
    # Merge vertex y into vertex x
    for v, weight in list(G[y].edges.items()):
        if v != x:
            G[x].addEdge(v, weight)
            G[v].addEdge(x, weight)
        G[v].delEdge(y)
    G[x].mergedInto.append(y)
    G[y].edges = {}
    G[y].merged = True


def minimumCutPhase(graph):
    # Find the first node with edges to start the phase
    start_node = None
    for i, node in enumerate(graph):
        if node.edges:
            start_node = i
            break

    if start_node is None:
        return float('inf'), None

    S = set()
    weights = [0] * len(graph)
    pq = PriorityQueue()
    pq.put((-weights[start_node], start_node))
    # This will track 2 last vertexes that were added to S
    tracker = (-1, -1)

    while len(S) < len(graph):
        while not pq.empty():
            _, current = pq.get()
            if current not in S:
                break
        else:
            break

        # add element with biggest cost to S to S
        S.add(current)
        tracker = (tracker[1], current)

        for neighbor, edge_weight in graph[current].edges.items():
            if neighbor not in S:
                weights[neighbor] += edge_weight
                pq.put((-weights[neighbor], neighbor))

    if len(S) < 2:
        return float('inf'), None

    s = tracker[0]
    t = tracker[1]

    cut_value = sum(weight for neighbor, weight in graph[t].edges.items() if neighbor in S)
    mergeVertices(graph, s, t)

    return cut_value, (s, t)


def stoerWagner(graph):
    min_cut = float('inf')
    best_cut = None

    while True:
        cut_value, cut_pair = minimumCutPhase(graph)

        if cut_value == float('inf'):
            break

        if cut_value < min_cut:
            min_cut = cut_value
            best_cut = cut_pair

        active_nodes = sum(1 for node in graph if node.edges)
        if active_nodes <= 1:
            break

    return best_cut, min_cut


def main(file_name):
    print("********")
    print("FileName: ", file_name)
    sol = {}
    n, edges = loadWeightedGraph(file_name)
    graph = [Node() for _ in range(n + 1)]
    for x, y, c in edges:
        graph[x].addEdge(y, c)
        graph[y].addEdge(x, c)

    while True:
        cut_pair, cut_value = stoerWagner(graph)
        if cut_value == float('inf'):
            break
        sol[cut_pair] = cut_value

    res = (None, float('inf'))
    for key, val in sol.items():
        if val < res[1]:
            res = (key, val)

    print("Correct solution: ", readSolution(file_name))
    print("Script solution: ", res[1])
    print("Passed: ", res[1] == int(readSolution(file_name)))
    return res

if __name__ == '__main__':
    tests = [join("graphs", f) for f in listdir("graphs") if isfile(join("graphs", f))]
    for test in tests:
        main(test)

