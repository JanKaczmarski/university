"""
Description of algorithm in ./heap_sort.pdf
"""

def parent(i):
    return (i - 1) // 2


def left(i):
    return 2 * i + 1


def right(i):
    return 2 * i + 2


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


def heap_sort(A):
    n = len(A)
    A = build_heap(A)
    
    for i in range(n - 1, 0, -1):
        A[i], A[0] = A[0], A[i]
        A = heapify(A, i, 0)

    return A


if __name__ == '__main__':
    a = [1, 2, 3, 4, 17, 5, 1, 9]
    print(heap_sort(a))
