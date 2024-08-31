def path1(S, b, L):
    curr_l = L
    result = 0
    i = 0
    prev_station = 0
    while i < b:
        if curr_l <= 0:
            i = prev_station
            curr_l = L
            result += 1
            continue   
        if S[i]:
            prev_station = i
        
        curr_l -= 1
        i += 1
        
    return result
        
        