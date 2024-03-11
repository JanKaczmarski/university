# Jan Kaczmarski
# Algorytm korzysta z MergeSorta zaimplementowanego na cwiczeniach
# z lekkimi poprawkami i sortuje LinkedListe
# funkcja n_series wybiera rosnące ciągi liczb od danego wskaznika
# funkcja merge laczy 2 posortowane linkedlisty a w usrednionym
# przypadku o podobnej dlugosci
# funkcja sort bierze Liste i wyciaga z niej po 2 rosnace ciagi
# za pomoca n_series i gdy c=2 uzywa funkcji merge zeby je polaczyc
# i dopiac na koniec listy

# dla k=Θ(1) Θ(nlogn)
# dla k=Θ(logn) Θ(nlogn)
# dla k=Θ(n) Θ(nlogn)
from zad1testy import Node, runtests

def n_series(L):
    guard = Node()
    tail = guard
    if L.next == None:
        return L
    value = L.next.val
    while L.next != None and L.next.val >= value:
        tail.next = L.next
        tail = tail.next
        L.next = L.next.next
        tail.next = None
        value = tail.val
    return guard, L


def merge(l1, l2):
    L = Node()
    tail = L

    while l1.next != None and l2.next != None:
        if l1.next.val >= l2.next.val:
            tail.next = l2.next
            l2.next = l2.next.next
        else:
            tail.next = l1.next
            l1.next = l1.next.next
        tail = tail.next
        tail.next = None
    if l1.next != None:
        tail.next = l1.next
        l1.next = None
    elif l2.next != None:
        tail.next = l2.next
        l2.next = None
    while tail.next != None:
        tail = tail.next
    return L, tail


def sort(L):
    while True:
        c = 0
        p = Node()
        tail = p
        while L.next != None:
            s1, L = n_series(L)
            c += 1
            if L.next != None:
                s2, L = n_series(L)
                c += 1
            else:
                s2 = Node()

            mgd, mgd_tail = merge(s1, s2)
            tail.next = mgd.next
            tail = mgd_tail
        L.next = p.next
        tail = p
        p.next = None
        if c == 1:
            break
    return L.next


def SortH(p, k):
    # stworz wartownika
    head = Node()
    head.next = p
    # przypisz dla p posortowana liste bez wartownika
    p = sort(head)
    return p


# zmien all_tests na True zeby uruchomic wszystkie testy
runtests(SortH, all_tests=True)


