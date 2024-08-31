from kolutesty import runtests

def partition(arr, left, right):
    x = arr[right]
    i = left - 1
    for j in range(left, right):
        if arr[j] <= x:
            i += 1
            arr[i], arr[j] = arr[j], arr[i]
    
    arr[i + 1], arr[right] = arr[right], arr[i + 1]
    
    return i + 1

def quicksort(arr, left, right):
    while left < right:
        q = partition(arr, left, right)
        quicksort(arr, left, q - 1)
        left = q + 1
    
    
    

def ice_cream( T ):
    # sorted in ascending order
    quicksort(T, 0, len(T) - 1)
    sol = 0
    gap = 0
    i = len(T) - 1
    while i >= 0 and T[i] - gap > 0:
        sol += T[i] - gap
        gap += 1
        i -= 1
        
    return sol

#T = [5, 1, 3, 7, 8]
#print(ice_cream(T))

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( ice_cream, all_tests = True )
