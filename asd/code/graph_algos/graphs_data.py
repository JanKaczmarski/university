# we only hold in here CONSTANTS values
# so there is no risk of impacting naming of other values in file
from constants import *

class GraphTestUnit:
    def __init__(self, input_graph, graph_repr):
        self.graph = input_graph
        if graph_repr not in POSS_GRAPH_REPR_GLOBAL:
            print("Wrong graph_repr")
            raise TypeError
        self.graph_repr = graph_repr
        self.expected = {}
    
    def relax(self, distance, u, v, weight, parent=[]):
        if distance[v] > distance[u] + weight:
            distance[v] = distance[u] + weight
            if parent:
                parent[v] = u
    
    def bellmanFord(self, start):
        if self.graph_repr not in POSS_GRAPH_REPR_PER_METHOD[BELLMAN_FORD]:
            print(f"Wrong graph_repr, choose one {POSS_GRAPH_REPR_PER_METHOD[BELLMAN_FORD]}")
            raise TypeError
        if self.graph_repr == 'l':
            return self.bellmanFord_l(start)
        
        # TODO matrix repr
        #elif self.graph_repr == 'm':
        #    return self.bellman_ford_m(start)

        
    
    def bellmanFord_l(self, start):
        # graph is adjecency list
        # and edges are like (weight, vertex)
        n = len(self.graph)
        distance = [float('inf') for _ in range(n)]
        parent = [None for _ in range(n)]
        distance[start] = 0
        
        for _ in range(n - 1):
            for u in range(n):
                for weight, v in self.graph[u]:
                    self.relax(distance, u, v, weight, parent)
        
        for u in range(n):
            for weight, v in self.graph[u]:
                if distance[v] > distance[u] + weight:
                    return [], []
        
        return distance, parent
    
    