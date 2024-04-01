def counting_sort(A, k):
    """
    A: array to sort
    k: A holds Natural numbers from {0,...,k-1}
    """
    n = len(A)
    # sorted data lands in B arr
    B = [None] * n
    # count number of different elems
    C = [0] * k
    # count number of occurences of number from {0,...,k-1} in A
    for x in A:
        C[x] += 1
    # C[i] hold how many elements are before i
    for i in range(1, k):
        C[i] += C[i - 1]

    # insert elems to their spot going from the end of array
    for i in range(n-1, -1, -1):
        B[C[A[i]] - 1] = A[i]
        C[A[i]] -= 1
    # copy sorted data to starting array
    for i in range(n):
        A[i] = B[i]
