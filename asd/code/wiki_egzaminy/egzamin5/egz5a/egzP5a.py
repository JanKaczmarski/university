from egzP5atesty import runtests 

def inwestor ( T ):
    # f(i, j) - solution starting from i going to j
    n = len(T)
    min_val = [[float('inf') for _ in range(n)] for _ in range(n)]
    result = 0
    
    # border values
    min_val[0][0] = T[0]
    for i in range(1, n):
        min_val[0][i] = min(T[i], min_val[0][i - 1])
        
    for i in range(n):
        for j in range(i, n):
            min_val[i][j] = min(min_val[i][j - 1], T[j])
            result = max(result, min_val[i][j] * (j - i + 1))

    return result
           
           
#T = [2, 1, 5, 6, 2, 3] 
#print(inwestor(T))

runtests ( inwestor, all_tests=True )
