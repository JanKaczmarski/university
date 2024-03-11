class Node:
    def __init__(self, val=None):
        self.val = val
        self.next = None


def list_to_ll(arr, guardian=False):
    p = Node()
    head = p

    for i in range(len(arr)):
        new = Node(arr[i])
        p.next = new
        p = p.next

    if guardian:
        return head

    return head.next


class DoubleNode(Node):
    def __init__(self, val=None, last=None):
        self.last = last
        super().__init__(val)


def print_list(p, guardian=False):
    if guardian is False:
        while p is not None:
            print(p.val)
            p = p.next
    else:
        while p.next is not None:
            print(p.next.val)
            p = p.next


def remove_element(p, to_delete):
    start = p

    while p.next != None:
        if p.next.val == to_delete:
            p.next = p.next.next
            return start
        p = p.next


def get_ll(*args, guardian=False):
    'len(args) == 6'
    g = Node()

    p = Node(args[0])
    a = Node(args[1])
    b = Node(args[2])
    c = Node(args[3])
    d = Node(args[4])
    e = Node(args[5])

    g.next = p

    p.next = a
    a.next = b
    b.next = c
    c.next = d
    d.next = e

    if guardian:
        return g
    return p


def print_cycle(p):
    first = p
    while True:
        print(p.val)
        p = p.next
        if p == first:
            return


def get_cycle(*args):
    "specify 6 values in args"
    cycle = Node(args[0])
    b = Node(args[1])
    c = Node(args[2])
    d = Node(args[3])
    e = Node(args[4])
    f = Node(args[5])

    cycle.next = b
    b.next = c
    c.next = d
    d.next = e
    e.next = f
    f.next = cycle

    return cycle


def asc_series(L):
    guard = Node(None)
    tail = guard
    if L.next == None: return L
    value = L.next.val
    while L.next != None and L.next.val >= value:
        tail.next = L.next
        tail = tail.next
        L.next = L.next.next
        tail.next = None
        value = tail.val
    return guard, L

def merge(l1, l2):
    L = Node(None)
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
    while tail.next != None: tail = tail.next
    return L, tail

def sort(L):
    while True:
        c = 0
        p = Node(None)
        tail = p
        while L.next != None:
            s1,L = asc_series(L)
            c += 1
            if L.next != None:
                s2,L = asc_series(L)
                c += 1
            else:
                s2 = Node(None)
            mgd,mgd_tail = merge(s1,s2)
            tail.next = mgd.next
            tail = mgd_tail
        L.next = p.next
        tail = p
        p.next = None
        if c == 1: break
    return L

def printl(L):
    p = L
    while p != None:
        print(p.val, end=", ")
        p = p.next
    print()
