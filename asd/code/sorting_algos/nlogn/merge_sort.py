class Node:
    def __init__(self, val=None):
        self.val = val
        self.next = None


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

