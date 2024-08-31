from kol1testy import runtests

# maxheap
def left(i):
    return 2 * i + 1
def right(i):
    return 2 * i + 2
def parent(i):
    return (i - 1) // 2

def increase_heap(H, i, x):
    p = parent(i)
    init_i = i
    H[i] = x
    while H[p] < x and i != 0:
        H[p], H[i] = H[i], H[p]
        i = p
        p = parent(i)
    H.append(0)
    return init_i - i
    
        
def maxrank(T):
    heap = [T[0], 0]
    sol = 0
    #res = increase_heap(heap, last_id, T[1])
    #print(heap, res)
    for i in range(1, len(T)):
        el = T[i]
        last_id = len(heap) - 1
        res = increase_heap(heap, last_id, el)
        sol = max(res, sol)
    return sol


            
    
# zmien all_tests na True zeby uruchomic wszystkie testy
#runtests( maxrank, all_tests = False )

# wywala sie w przykladzie ponizej, czyli kiedy dziecko tego samego poziomu jest 
# od niego mniejsze
if __name__ == '__main__':
    arr = [5, 3, 4]
    print(maxrank(arr))

