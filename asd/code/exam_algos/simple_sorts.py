
# bubble sort
def bsort(t):
    n = len(t)
    for i in range(n):
        for j in range(n - i - 1):
            if t[j] > t[j+1]:
                t[j], t[j+1] = t[j+1], t[j]
    return t


# liniowa zlozonosc dla malych tablic
# uzywa sie tego w realu dla malych tablic
def insertion_sort(t):
    n = len(t)
    for i in range(1, n):
        for j in range(i, 0, -1):
            if t[j] < t[j - 1]:
                t[j], t[j-1] = t[j-1], t[j]
            else:
                break
    return t


# selection sort
def ssort(t):
    n = len(t)
    for i in range(n):
        idx = i
        for j in range(i, n):
            if t[j] < t[idx]:
                idx = j
                
        t[i], t[idx] = t[idx], t[i]
    return t