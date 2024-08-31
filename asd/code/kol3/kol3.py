from kol3testy import runtests


def orchard(T, m):
    n = len(T)
    # False - can't solve
    # True - can solve
    # None - hasn't been computed yet
    dp = [[[None for _ in range(m)] for __ in range(n)] for _ in range(n)]
    # border values for dp[0]
    dp[0][0][0] = True
    for i in range(n):
        for j in range(m):
            if i == 0 and j != 0:
                dp[0][i][j] = False
            elif i > 0:
                if T[0] == j: dp[0][i][j] = True
                elif T[0] != j: dp[0][i][j] = False
    
    def f(i, k, mod):
        # edge case, we can't chop < 0 trees
        if k < 0:
            return None
        
        
        # if value is already computed True or False
        if dp[i][k][mod] is not None:
            return dp[i][k][mod]
              
        # we chop the i-th tree or not
        tmp = mod
        mod = (mod + T[i]) % m
        dp[i][k][tmp] = f(i - 1, k, mod) or f(i - 1, k - 1, tmp)
        
        return dp[i][k][tmp]
    
    # special case
    res = f(n-1, 0, 0)
    if res: return 0
    
    # read the solution
    for k in range(1, n):
        res = f(n-1, k, 0)
        if res:
            return k + 1
        
    

# najmniejsza liczba drzew do wyciecia jesli
# startujemy z i, wycielismy do tej pory k drzew i nasza mod to aktualna reszta
# f(i, k, mod)

# i == len(T) -> k lub nic
# dp[i][k][mod] != inf -> return dp[i][k][mod]

#T	=  [2, 2, 7, 5, 1, 14, 7]
#m = 7
#print(orchard(T, m))

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests(orchard, all_tests=True) 
