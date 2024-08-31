def invalid_tile(x, y, n, L):
    # out of bounds or blocked cell
    if x >= n or x < 0 or y >= n or y < 0 or L[x][y] == '#':
        return True
    return False

def maze(L):
    n = len(L)
    g_arr = [[None for __ in range(n)] for _ in range(n)]
    d_arr = [[None for __ in range(n)] for _ in range(n)]

    def f(x, y):
        if invalid_tile(x, y, n, L):
            return float('-inf')
        if x == y == 0:
            return 0
        
        if g_arr[x][y] is not None:
            g_value = g_arr[x][y]
        else:
            g_value = g(x, y)
        
        if d_arr[x][y] is not None:
            d_value = d_arr[x][y]
        else:
            d_value = d(x, y)
        
        result = max(g_value, d_value)
        print(f"f({x}, {y}) = {result}")
        return result

    def g(x, y):
        if invalid_tile(x, y, n, L):
            return float('-inf')
        if x == y == 0:
            return 0
        if g_arr[x][y] is not None:
            return g_arr[x][y]

        g_arr[x][y] = max(f(x-1, y), g(x, y-1)) + 1
        print(f"g({x}, {y}) = {g_arr[x][y]}")
        return g_arr[x][y]

    def d(x, y):
        if invalid_tile(x, y, n, L):
            return float('-inf')
        if x == y == 0:
            return 0
        if d_arr[x][y] is not None:
            return d_arr[x][y]

        d_arr[x][y] = max(f(x-1, y), d(x, y+1)) + 1
        print(f"d({x}, {y}) = {d_arr[x][y]}")
        return d_arr[x][y]

    sol = f(n - 1, n - 1)
    if sol == float('-inf'):
        return -1

    return sol

# Example test case
L = ["....", "..#.", "..#.", "...."]
print(maze(L))  # Output should be the maximum number of rooms visited