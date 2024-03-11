"""
Find diff in sorted array
"""

def find(T, val):
    n = len(T)
    i, j = 0
    while i < n and j < n:
        if T[i] - T[j] == val:
            return i,j 
        elif T[i] - T[j] < val:
            i += 1
        else:
            j += 1
    
    return None

