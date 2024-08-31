from egzP6atesty import runtests 

def partition(A, l, r):
    x = len(A[r])
    i = l - 1
    for j in range(l, r):
        if len(A[j]) >= x:
            i += 1
            A[i], A[j] = A[j], A[i]
    A[i + 1], A[r] = A[r], A[i + 1]
    
    return i + 1


def quicksort(A, p, r):
    while p < r:
        q = partition(A, p, r)
        quicksort(A, p, q-1)
        p = q + 1


def google ( H, s ):
    quicksort(H, 0, len(H) - 1)
    return H[s - 1]


#runtests ( google, all_tests=True )