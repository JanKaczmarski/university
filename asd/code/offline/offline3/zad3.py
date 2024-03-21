from zad3testy import runtests


def quicksort(T, p, r, z):
    # s imituje stos
    # na stosie umieszczam zadania do wykonania
    s = []
    s.append((p, r))

    # dopuki cokolwiek na stosie wykonuj
    while len(s) != 0:
        # get p and r from stack
        p, r = s.pop()
        # get pivot
        q = partition(T, p, r, z)

        # jesli chunk nie jest pusty to dodaj na stos
        if p < q-1:
            s.append((p, q-1))
        if q + 1 < r:
            s.append((q + 1, r))


def partition(A, p, r, z):
    x = A[r]
    i = p-1
    for j in range(p, r):
        if A[j][z] <= x[z]:
            i += 1
            A[i], A[j] = A[j], A[i]

    A[i + 1], A[r] = A[r], A[i + 1]

    return i + 1

# can try using k to get undecided elements
# but i will try to sort and then check how many dominated by A[-1]

def s_d_partition(A, p, r):
    x = A[r]
    i = p-1
    for j in range(p, r):
        # A[j] > x -> False
        # A[j] < x -> True
        # d_state = is_dominering(A[j], x)
        # if A[j] is dominated by pivot
        if A[j][0] <= x[0]:
            i += 1
            A[i], A[j] = A[j], A[i]

    A[i + 1], A[r] = A[r], A[i + 1]

    return i + 1


def f_d_partition(A, p, r):
    x = A[r]
    i = p-1
    for j in range(p, r):
        # A[j] > x -> False
        # A[j] < x -> True
        d_state = is_dominering(A[j], x)
        # d_state = is_dominering(A[j], x)
        # if A[j] is dominated by pivot
        if A[j][0] <= x[0]:
            i += 1
            A[i], A[j] = A[j], A[i]

    A[i + 1], A[r] = A[r], A[i + 1]

    return i + 1

def d_partition(A, p, r):
    x = A[r]
    i = p-1
    for j in range(p, r):
        # A[j] > x -> False
        # A[j] < x -> True
        d_state = is_dominering(A[j], x)
        # d_state = is_dominering(A[j], x)
        # if A[j] is dominated by pivot
        if d_state == True:
            i += 1
            A[i], A[j] = A[j], A[i]
        elif d_state == None:

    A[i + 1], A[r] = A[r], A[i + 1]

    return i + 1


def d_sort(T, p, r):
    # s imituje stos
    # na stosie umieszczam zadania do wykonania
    s = []
    s.append((p, r))

    # dopuki cokolwiek na stosie wykonuj
    while len(s) != 0:
        # get p and r from stack
        p, r = s.pop()
        # get pivot
        q = d_partition(T, p, r)

        # jesli chunk nie jest pusty to dodaj na stos
        if p < q-1:
            s.append((p, q-1))
        if q + 1 < r:
            s.append((q + 1, r))


def is_dominering(a, b):
    """
    is b dominering a
    """
    if a[0] > b[0] and a[1] > b[1]:
        return False
    elif a[0] < b[0] and a[1] < b[1]:
        return True
    return None


def dominance(P):
    n = len(P)
    d_sort(P, 0, n - 1)
    dominant = P[-1]
    sol = n - 1
    print(P)
    for i in range(n - 1, -1, -1):
        status = is_dominering(P[i], dominant)
        if status == None and is_bigger(dominant, P[i]):
            sol -= 1
        # We have met actuall dominant 
        elif status == False:
            dominant = P[i]
    return sol

# zmien all_tests na True zeby uruchomic wszystkie testy
# runtests( dominance, all_tests = True )


if __name__ == '__main__':
    print(dominance([(2, 7), (6, 7), (6, 3), (10, 9), (2, 3), (10, 5), (10, 1)]))
    #print(dominance([(2, 4), (2, 1), (5, 5), (6, 4), (1, 2)]))
    

# Notes
# Posortuj wzgldem x[0] lub x[1] whatever
