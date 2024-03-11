# in this sort we sort linked lists

class Node:
    def __init__(self, x):
        self.val = x
        self.next = None


def n_series(L):
    res = Node(None)
    tail = res
    
    if L.next == None:
        return res
    
    val = L.next.val
    while L.next != None and L.next.val >= val:
        tail.next = L.next
        tail = tail.next
        L.next = L.next.next
        tail.next = None
        val = tail.val    
    
    return res, L
    
    
def merge(a, b):
    L = Node(None)
    tail = L
    while a.next != None and b.next != None:
        if a.next.val >= b.next.val:
            tail.next = b.next
            b = b.next
        
        else:
            tail.next = a.next
            a = a.next
        
        tail.next = None
        
    if a.next != None:
        tail.next = a.next
        a.next = None
    if b.next != None:
        tail.next = b.next
        b.next = None
    
    while tail.next != None:
        tail = tail.next
    
    return L, tail
        
        
def l_sort(L):
    p = Node(None)
    while True:
        c = 0
        p = Node(None)
        tail = p
        
        while L.next != None:
            s1, L = n_series(L)
            c += 1
            if L.next != None:
                s2, L = n_series(L)
                c += 1
            else:
                s2 = Node(None)
            
            m, m_tail = merge(s1, s2)
            # Jak bylo na zajeciach
            tail.next = m.next
            # p.next = m.next
            tail = m_tail
        
        L.next = p.next
        tail = p
        p.next = None
        
        if c <= 2:
            break
    
    return L
        
    