# min i max dla tablicy przy 3/2n operacji

def minmax(t):
    # last element because in len(t) is uneven we will skip last
    minimum = t[-1]
    maximum = t[-1]
    
    for i in range(0, len(t) - 1, 2):
        if t[i] > t[i + 1]:
            if t[i] > maximum:
                maximum = t[i]
            if t[i + 1] < minimum:
                minimum = t[i + 1]
        else:
            if t[i + 1] > maximum:
                maximum = t[i + 1]
            if t[i] < minimum:
                minimum = t[i]
        
    return minimum, maximum


# n^2
def even_sets_2(t):
    n = len(t)
    cnt = 0
    sp = [None] * (n+1)
    sp[0] = 0
    s = 0
    for i in range(n):
        s += t[i]
        sp[i+1] = s
    
    # zliczamy w petli reszte, TODO
    
    return cnt



# time complexity is O(n)
def even_sets(t):
    n = len(t)
    cnt = 0
    sp = [None] * (n+1)
    odd = [None] * (n + 1)
    even = [None] * (n + 1)
    sp[0] = 0
    s = 0
    
    for i in range(n):
        s += t[i]
        sp[i + 1] = s

    even_cnt = 0
    odd_cnt = 0
            
    for j in range(n+1):
        if sp[j] % 2 == 0:
            even_cnt += 1
        else:
            odd_cnt += 1
        even[j] = even_cnt
        odd[j] = odd_cnt
    
    for k in range(n+1):
        if sp[k] % 2 == 0:
            cnt += even[k]
        else:
            cnt += odd[k]
    
    return cnt


def min_excluded(t):
    l ,r = t[0], t[len(t) - 1] + 1
    m = 0
    while l < m:
        m = (l + r) // 2
        if t[m] == m:
            l = m
        else:
            r = m
    
    if t[m + 1] - t[m] > 1:
        return t[m] + 1
            