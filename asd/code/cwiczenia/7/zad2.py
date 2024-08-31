"""
Mamy tablice parentow i mamy odtworzyc sciezke z a do b
"""

def print_path(parent, s, e):
    u = e
    res = [u]
    while u != s:
        res = [parent[u]] + res
        u = parent[u]

    return res
    