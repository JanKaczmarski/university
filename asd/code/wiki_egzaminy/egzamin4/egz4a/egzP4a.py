from egzP4atesty import runtests 

def mosty ( T ):
    # sort by first num in tuple
    T.sort(key=lambda x:(x[0], x[1]))
    n = len(T)
    dp = [0 for _ in range(n)]
    dp[0] = 1
    
    for i in range(1, n):
        for k in range(i):
            if T[k][1] <= T[i][1]:
                dp[i] = max(dp[i], dp[k] + 1)
                
            
    return max(dp)

runtests ( mosty, all_tests=True )
