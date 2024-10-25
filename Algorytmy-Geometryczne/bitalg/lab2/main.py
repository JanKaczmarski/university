# How to categorize if point is smaller (less angle)
# Bigger if bigger angle
# 3 points -> base_point, x axis -> (base_point[0] + 1, base_point[1]), i-th point = value of 

# punkt a i b -> punkt bazowy q
# jesli a jest > b to det(q, a, b) jest > 0

def mat_det_3x3(a, b, c):
    """
    Obliczanie wyznacznika macierzy 3x3 bez użycia funkcji bibliotecznych
    :param a: krotka współrzędnych (x, y) pierwszego punktu tworzącego naszą prostą
    :param b: krotka współrzędnych (x, y) drugiego punktu tworzącego naszą prostą
    :param c: krotka współrzędnych (x, y) punktu, którego położenie względem prostej chcemy znaleźć
    :return: wartość wyznacznika macierzy
    """
    return (b[0] - a[0])*(c[1]-b[1]) - (b[1]-a[1])*(c[0]-b[0])

def partition(A, p, r, base):
    x = A[r]
    i = p - 1
    for j in range(p, r):
        if mat_det_3x3(base, A[j], x) > 0:
            i += 1
            A[i], A[j] = A[j], A[i]

    A[i + 1], A[r] = A[r], A[i + 1]
    return i + 1



def trig_sort(A, p, r, base):
    while p < r:
        q = partition(A, p, r, base)
        trig_sort(A, p, q-1, base)
        p = q + 1
    
A = [(0,0),(1,1),(1,0),(-1,2)] 
trig_sort(A, 1, len(A)-1, (0,0))
print(A)

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
"""
