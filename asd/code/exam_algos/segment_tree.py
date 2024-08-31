class Node:
    def __init__(self, val=0, parent=None, span=(float('-inf'), float('inf'))):
        self.left = None
        self.right = None
        self.span = span
        self.val = val
        self.parent = parent
        self.elements = []
    
    def insert_segment(self, segment):
        if self.span[0] <= segment[0] and segment[1] <= self.span[1]:
            self.elements.append(segment)
        elif self.left:
            self.left.insert(segment)
        elif self.right:
            self.right.insert(segment)
    
    def get_affiliation(self, a):
        if self.left and self.val > a:
            self.left.get_affiliation(a)
        elif self.right and self.val < a:
            self.right.get_affiliation(a)
        print(self.val)


class BST:
    def __init__(self, val):
        self.root = Node(val)
    
    def add_node(self, parent_node, val):
        if parent_node.val == val:
            return
        if parent_node.val < val:
            if parent_node.right:
                self.add_node(parent_node.right, val)
            else:
                parent_node.right = Node(val, parent_node, span=(parent_node.val, parent_node.span[1]))
        elif parent_node.val > val:
            if parent_node.left:
                self.add_node(parent_node.left, val)
            else:
                parent_node.left = Node(val, parent_node, span=(parent_node.span[0], parent_node.val))
            
                
