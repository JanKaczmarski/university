
# neighbours arrays
def dfs(G):
    n = len(G)
    visited = [False for _ in range(n)]
    # at what time which vertex was processed
    time_arr = [-1 for _ in range(n + 1)]
    time = 0
    
    def dfsVisit(G, u):
        nonlocal visited, time, time_arr
        
        for v in G[u]:
            if not visited[v]:
                visited[v] = True
                dfsVisit(G, v)

        time_arr[time] = u
        time += 1
    
    
    for u in range(n):
        if not visited[u]:
            dfsVisit(G, u)

            
    
    return visited, time_arr, time

if __name__ == '__main__':
    a = [
        [1, 4], # a
        [2], # b
        [0, 7], # c
        [4, 6], # d
        [5], # e
        [3], # f
        [5], # g
        [8], # h
        [3, 9], # i
        [5, 10], # j
        [7], # k
    ]
    
    print(dfs(a))
    