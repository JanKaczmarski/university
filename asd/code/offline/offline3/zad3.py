from zad3testy import runtests


def c_sort(T, p, m):
    """
    T = [(2,3), (11,5), (6,93), ...]    
    m -> possible max(T)
    p -> sort by which element in array
    """
    n = len(T)
    C = [0 for _ in range(m + 1)]
    for el in T:
        C[el[p]] += 1

    for i in range(1, m + 1):
        C[i] += C[i - 1]
    B = [None] * n

    for i in range(n-1, -1, -1):
        x = T[i][p]
        pos = C[x] - 1
        B[pos] = T[i]
        C[x] -= 1
    

    return B

def get_max(T):
    max0 = max1 = 0
    for el in T:
        max0 = max(max0, el[0])
        max1 = max(max1, el[1])
    return max0, max1

def is_dominant(a, b):
    """
    is a dominating b
    if a > b -> True
    if b > a -> False
    undefined -> None
    """
    if a[0] > b[0] and a[1] > b[1]:
        return True
    elif a[0] < b[0] and a[1] < b[1]:
        return False
    return None

def cnt(T, d, idx):
    sol = idx
    for i in range(idx - 1, -1, -1):
        if is_dominant(d, T[i]) is None:
            sol -= 1
    return sol

def dominance(P):
    n = len(P)
    max0, max1 = get_max(P)
    P = c_sort(P, 1, max1)
    P = c_sort(P, 0, max0)

    poss_d = []
    dominant = P[-1]
    sol = 0
    for i in range(n - 1, -1, -1):
        status = is_dominant(dominant, P[i])
        if status is None:
            poss_d.append((P[i], i))
        if status:
            break
    for d in poss_d:
        sol = max(sol, cnt(P, d[0], d[1]))
            
    return sol
    

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( dominance, all_tests = True )


#if __name__ == '__main__':
    #T = [(1, 3), (3, 4), (4, 2), (2, 2)]
    #print(dominance(T))
    #print(dominance([(2, 4), (2, 1), (5, 5), (6, 4), (1, 2)]))
    

# Notes
# Posortuj wzgldem x[0] lub x[1] whatever
