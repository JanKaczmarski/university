def partition(A, p, r):
    x = A[r]
    i = p-1
    for j in range(p, r):
        if A[j] <= x:
            i += 1
            A[i], A[j] = A[j], A[i]

    A[i + 1], A[r] = A[r], A[i + 1]

    return i + 1


def select(A, p, r, k):
    if p == r:
        return A[p]
    q = partition(A, p, r)
    if q == k:
        return A[q]
    elif k < q:
        return select(A, p, q-1, k)
    else:
        return select(A, q + 1, r, k)


if __name__ == '__main__':
    print(select([7, 9, 1, 5, 8, 6, 2, 12], 0, 4, 1))

    