from zad7testy import runtests

"""
Dla kazdej komnaty mamy tuple (left, up, down)
co mowi nam o max. koszcie dotarcia do aktualnej komnaty
max. koszt dotarcia do (n-1, n-1) to max(dp[n-1][n-1])
"""

"""
BRUTE FORCE

def maze( L ):
    n = len(L)
    # dist from origin for (i, j) point
    dp = [[0 for __ in range(n)] for _ in range(n)]
    parent = [[(None, None) for _ in range(n)] for __ in range(n)]
    q = deque()
    q.append((0, 0))
    while len(q) > 0:
        i, j = q.popleft()

        if L[i][j] == '#':
            continue

        # right
        # in bounds, we can't travel to parent, increasing path value
        if j + 1 < n and parent[i][j] != (i, j + 1) and dp[i][j] + 2 > dp[i][j + 1]:
            parent[i][j + 1] = (i, j)
            dp[i][j + 1] = dp[i][j] + 1
            q.append((i, j + 1))
        if i + 1 < n and parent[i][j] != (i + 1, j) and dp[i][j] + 2 > dp[i + 1][j]:
            parent[i + 1][j] = (i, j)
            dp[i + 1][j] = dp[i][j] + 1
            q.append((i + 1, j))
        if i - 1 >= 0 and parent[i][j] != (i - 1, j) and dp[i][j] + 2 > dp[i - 1][j]:
            parent[i -1][j] = (i, j)
            dp[i - 1][j] = dp[i][j] + 1
            q.append((i - 1, j))
    
    return dp[n-1][n-1]
        
    
        
    
"""
def invalid_tile(x, y, n, L):
    # out of bounds
    if x > n-1 or x < 0:
        return True
    if y > n-1 or y < 0:
        return True
    if L[x][y] == '#':
        return True
    
    # in bounds
    return False


def maze( L ):
    n = len(L)
    g_arr = [[0 for __ in range(n)] for _ in range(n)]
    d_arr = [[0 for __ in range(n)] for _ in range(n)]
    
    def f(x,y):
        if invalid_tile(x, y, n, L):
            return float('-inf')  
        if x == y == 0:
            return 0
    
        return max(d(x,y), g(x,y))
    
    def g(x, y):
        nonlocal g_arr
        if invalid_tile(x, y, n, L):
            return float('-inf')
        if x == y == 0:
            return 0
        if g_arr[x][y] != 0:
            return g_arr[x][y]
        
        g_arr[x][y] = max(f(x-1,y), g(x,y-1)) + 1
        
        return g_arr[x][y]
    
    def d(x, y):
        nonlocal d_arr
        if invalid_tile(x, y, n, L):
            return float('-inf')
        if x == y == 0:
            return 0
        if d_arr[x][y] != 0:
            return d_arr[x][y]
        
        d_arr[x][y] = max(f(x-1,y), d(x,y+1)) + 1
        
        return d_arr[x][y]
    
    sol = f(n - 1, n - 1)
    if sol == float('-inf'):
        return -1
    
    return d_arr, g_arr, sol

# zmien all_tests na True zeby uruchomic wszystkie testy
#runtests( maze, all_tests = False )
L = [ "....","..#.","..#.","...." ]
print(maze(L))


