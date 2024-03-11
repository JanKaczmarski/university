from zad1testy import Node, runtests
from support import list_to_ll, printl, sort


def k_series(L, k):
    guard = Node()
    tail = guard
    if L.next == None:
        return L
    cnt = 0
    while L.next != None and cnt < k:
        tail.next = L.next
        tail = tail.next
        L.next = L.next.next
        tail.next = None
        cnt += 1
    return guard, L


def SortH(p, k):
    g = Node()
    tail = g
    a, p = k_series(p, 2 * (k + 1))
    a = sort(a)
    left, right = k_series(a, k+1)
    tail.next = left
    while tail.next != None:
        tail = tail.next
    tail.next = None
    
    
    return 


# runtests( SortH, all_tests=True )

a = list_to_ll([2,0,1,5,4,3,7,9,6,8], guardian=True)
printl(SortH(a, 1))

"""
if __name__ == '__main__':
    #Mega close zaraz to mam jutro rano powinno sie udac
    k = 1
    a = list_to_ll([2,0,1,5,4,3,7,9,6,8], guardian=True)
    g, a = k_series(a, 2 * (k + 1))
    printl(g)
    g= sort(g)
    printl(g)
    g, b = k_series(g, k+1)
    printl(b)
"""
