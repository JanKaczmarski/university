from collections import deque

def bfs(G, start=0):
    n = len(G)
    visited = [False for _ in range(n)]
    parent = [None for _ in range(n)]
    
    queue = deque()
    visited[start] = True
    
    queue.append(start)
    
    while len(queue) > 0:
        u = queue.popleft()
        
        for v in G[u]:
            if not visited[v]:
                visited[v] = True
                parent[v] = u
                queue.append(v)    
    
    return visited, parent


if __name__ == '__main__':
    A = [
        [0, 1, 1, 0],
        [1, 0, 0, 1],
        [1, 0, 0, 0],
        [0, 1, 0, 0]
    ]
    a = [
        [1, 2],
        [0,3],
        [0],
        [1]
    ]

    print(bfs(a))