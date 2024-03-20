# Jan Kaczmarski
# Na początku posortuj wycinek z0 za pomocą quicksorta
# Nastepnie w petli dodajemy i usuwamy zmieniajace sie elementy
# Wyszukujemy indeksy na ktorych powinny sie znalez za pomoca binary_search
# I nastepnie ze zmienionego kawalka zi na zi+1 bierzemy k-ty najwiekszy
# element i dodajemy go do sumy
# time complexity = O(n^2)

from zad2testy import runtests

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


def partition(A, p, r):
    x = A[r]
    i = p-1
    for j in range(p, r):
        if A[j] <= x:
            i += 1
            A[i], A[j] = A[j], A[i]

    A[i + 1], A[r] = A[r], A[i + 1]

    return i + 1


def bin_s(T, x):
    high = len(T) - 1
    low = 0
    mid = 0
    while low <= high:
        mid = (low + high)//2
        if T[mid] < x:
            low = mid + 1
        elif T[mid] > x:
            high = mid - 1
        else:
            return mid
    
    return low
        
        
def ksum(T, k, p):
    n = len(T)
    # create slice that we'll be operating on
    z = T
    z = z[:p]
    # sort this slice to easily find k-th elem
    # time complexity is constant
    quicksort(z, 0, len(z) - 1)
    sol = z[-k]
    for i in range(1, n - p + 1):
        # remove and add changing elements
        # values
        to_del = T[i - 1]
        to_add = T[i + p - 1]
        # indx
        rmi = bin_s(z, to_del)
        
        # remove elem from list - O(n)
        del z[rmi]
        addi = bin_s(z, to_add)
        # add to list - O(n)
        z[addi:addi] = [to_add]
        
        # add k-th elem to sol
        sol += z[-k]
        
    return sol


# zmien all_tests na True zeby uruchomic wszystkie testy
runtests(ksum, all_tests=True)
