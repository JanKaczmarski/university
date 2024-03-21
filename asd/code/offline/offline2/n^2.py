from zad2testy import runtests

def k_th(T, diff):
    # TIME COMPLEXITY O(n)
    # diff means p - k
    cnt = 0
    while cnt < diff:
        T.remove(min(T))
        cnt += 1
        
    return min(T)
    
            

def ksum(T, k, p):
    n = len(T)
    sol = 0
    for i in range(n-p + 1):
        sol += k_th(T[i:i+p], p - k)
        
            
    return sol


# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( ksum, all_tests=True )
