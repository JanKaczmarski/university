
"""
1. Iteration using stack
"""
# mniejsza polowe po partition robimy rekurencja
# a dlusza iteracja => w pesymistycznym przypadku glebokosc
# rekurencji to log(n)

def partition(A, p, r):
    x = A[r]
    i = p-1
    for j in range(p, r):
        if A[j] <= x:
            i += 1
            A[i], A[j] = A[j], A[i]

    A[i + 1], A[r] = A[r], A[i + 1]

    return i + 1


# quicksort z rek_depth <= log(n)
def sort(T, p, r):
    while p < r:
        q = partition(T, p, r)
        # prawa czesc jest krotsza
        if q > (p + r) // 2:
            # sortujemy mniejsza czesc rekurencyjnie
            sort(T, q + 1, r)
            # lewa strona interacyjnie
            r = q - 1
        # lewa czesc jest krotsza
        else:
            sort(T, p, q - 1)
            p = q + 1
            

# Quick sort bez rekurencji
def quicksort(T, p, r):
    # s imituje stos
    # na stosie umieszczam zadania do wykonania
    s = []
    s.append((p, r))

    # dopuki cokolwiek na stosie wykonuj  
    while len(s) != 0:
        # get p and r from stack
        p, r = s.pop()
        # get pivot
        q = partition(T , p, r)

        # jesli chunk nie jest pusty to dodaj na stos
        if p < q-1:
            s.append((p,q-1))
        if q + 1 < r:
            s.append((q + 1, r))
            
            
"""
2. Recursion
"""
def quicksort(A, p, r):
    while p < r:
        q = partition(A, p, r)
        quicksort(A, p, q-1)
        p = q + 1


def partition(A, p, r):
    x = A[r]
    i = p-1
    for j in range(p, r):
        if A[j] <= x:
            i += 1
            A[i], A[j] = A[j], A[i]

    A[i + 1], A[r] = A[r], A[i + 1]

    return i + 1