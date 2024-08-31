# insert n to minheap

def parent(n):
    if n == 0:
        return 0
    return (n - 1) // 2


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
        
