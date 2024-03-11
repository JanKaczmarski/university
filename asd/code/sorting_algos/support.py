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
