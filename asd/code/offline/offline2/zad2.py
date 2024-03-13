from zad2testy import runtests

def k_th(T, diff):
    # TIME COMPLEXITY O(n)
    # diff means p - k
    cnt = 0
    while cnt < diff:
        mini = min(T)
        if cnt == diff - 1:
            k_next = mini
        T.remove(mini)
        cnt += 1
            
    

    return [min(T), k_next]
    
            

def ksum(T, k, p):
    # get status of first chunk, z0
    # status = [i-th element, k-th element]
    diff = p - k
    status = [T[0]] + k_th( T[:p], diff )
    n = len(T)
    sol = status[1]
    
    for i in range(1, n-p + 1):    
        last = T[i + p - 1]
        if last >= status[1] and status[0] >= last:
            sol += status[1]
        #elif last < status[0] and last < status[1]:
        else:
            status = [T[i]] + k_th( T[i: i + p], diff )
            sol += status[1]
        status[0] = T[i]    
           
    return sol


# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( ksum, all_tests=True )

#print(ksum([7,9,1,5,8,6,2,12], 4, 5))


"""
Zapisac zmieniajace sie wartosci
Wysłano
Czyli i zamienia sie na p + i + 1
Wysłano
I mize jeszcze k-ty epement w przedziale
Wysłano
I 3 ify ?
Wysłano
Czyli funkcja dostaje liste i ty, k-1, k ty, k+1, 

I porownujemy z nowym elementem
"""