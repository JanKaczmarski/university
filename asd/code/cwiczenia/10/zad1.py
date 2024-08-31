def points(X):
    # przedzialy jednostkowe to przedzialy o dlugosci 1
    n = len(X)
    cnt = 0
    X.sort()
    start = X[0]
    for i in range(1, n):
        if X[i] > X[start] - 1:
            start = i
            cnt += 1
    
    return cnt    