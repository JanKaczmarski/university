# algorytm Lomuto
# istnieje też algorytm Hoare, który był na np. ćwiczeniach u Karatkevicha
def partition(A, p, r):
    x = A[r]
    i = p-1
    for j in range(p, r):
        if A[j] <= x:
            i += 1
            A[i], A[j] = A[j], A[i]

    A[i + 1], A[r] = A[r], A[i + 1]

    return i + 1

"""
select sort - k-ty element w posortowanej tablicy
"""
# O(n), na bazie quicksorta
def select(A, p, r, k):
    if p == r: return A[p]
    q = partition(A, p, r)
    if   k == q: return A[q]
    elif k <  q: return select(A, p, q-1, k)
    else:        return select(A, q+1, r, k)

