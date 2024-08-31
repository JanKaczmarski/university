

def euler_matrix(G):
    n = len(G)
    s = 0
    # for each vertex we keep track of how many 
    # edges did we chceck already
    visited_edges = [0] * n
    
    cycle = []
    
    def dfs_visit(G, s):
        nonlocal visited_edges, cycle
        while visited_edges[s] < n:
            # if there is edge
            if G[s][visited_edges[s]]:
                G[s][visited_edges[s]] = G[visited_edges[s]][s] = 0
                visited_edges[s] += 1
                dfs_visit(G, visited_edges[s] - 1)
            else:
                visited_edges[s] += 1
        cycle.append(s)
        
    dfs_visit(G, s)
    return cycle


G = [[0,1,1],
     [1,0,1],
     [1,1,0]]

print(euler_matrix(G))