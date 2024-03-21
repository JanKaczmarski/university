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


if __name__ == '__main__':
    data = [4, 3, 0, 1, 8, 9, 5]
    quicksort(data, 0, len(data) - 1)
    print(data)
