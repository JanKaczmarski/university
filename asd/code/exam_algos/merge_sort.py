
"""
1. Linked list merge sort
"""
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


"""
2. Array merge_sort
DOESN'T WORK !!!!
"""

def mergeSort(A, left, right):
    
    def merge(A, left, mid, right):
        # lengths of each part Left and Right
        len1 = mid - left + 1
        len2 = mid - right
        # Get slice for left part and right part
        left_a = A[left:mid+1]
        right_a = A[mid+1:right+1]
        # id's to track where are we in our 3 arrays:
        # A, left_a, right_a
        left_id = right_id = 0
        main_id = left
        
        # if we have elements in both arrays
        while left_id < len1 and right_id < len2:
            # if left < right insert to A[main_id]
            if left_a[left_id] <= right_a[right_id]:
                A[main_id] = left_a[left_id]
                left_id += 1
            # if right > left
            else:
                A[main_id] = right_a[right_id]
                right_id += 1
            # increment main_id so to insert to new id
            main_id += 1
            
        
        # There are always elements left in one of arrays
        # so use while to insert them to main arr
        while left_id < len1:
            A[main_id] = left_a[left_id]
            left_id += 1
            main_id += 1
        while right_id < len2:
            A[main_id] = right_a[right_id]
            right_id += 1
            main_id += 1
        
    
    if left < right:
        mid = (left + right) // 2
        mergeSort(A, left, mid) # sort left
        mergeSort(A, mid + 1, right) # sort right
        merge(A, left, mid, right)
        
        
if __name__ == '__main__':
    from random import randint
    a = [randint(1,100) for _ in range(10)]
    print(a)
    mergeSort(a, 0, len(a) - 1)
    print(a)
    