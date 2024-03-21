"""
0. Support functions
"""
def parent(i):
    return (i - 1) // 2


def left(i):
    return 2 * i + 1


def right(i):
    return 2 * i + 2


"""
1. minheap
"""
# insert n to minheap
def decrease_heap(T, i, x):
    # zal: x > T[i]
    p = parent(i)
    T[i] = x
    while T[p] > T[i]:
        T[p], T[i] = T[i], T[p]
        i = p
        p = parent(i)
        

class Heap:
    def __init__(self, max_size):
        self.max_size = max_size
        self.T = [None] * max_size
        self.size = 0
        
    # there shouldn't be H in here
    # we should just adhere to self
    def heap_add(self, H, x):
        if H.size < H.max_size:
            decrease_heap(H.T, H.size, x)
            H.size += 1
        
"""
2. maxheap
"""

# Build maxheap
def heapify(A, n, i):
    l = 2*i + 1
    r = 2*i + 2
    max_ind = i

    if l < n and A[l] > A[max_ind]:
        max_ind = l
    if r < n and A[r] > A[max_ind]:
        max_ind = r

    if max_ind != i:
        A[i], A[max_ind] = A[max_ind], A[i]
        A = heapify(A, n, max_ind)

    return A


def build_heap(A):
    n = len(A)

    for i in range(parent(n-1), -1, -1):
        A = heapify(A, n, i)

    return A


# HeapSort
def heap_sort(A):
    n = len(A)
    A = build_heap(A)
    
    for i in range(n - 1, 0, -1):
        A[i], A[0] = A[0], A[i]
        A = heapify(A, i, 0)

    return A
