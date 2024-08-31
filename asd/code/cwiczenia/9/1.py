
def main(arr):
    n = len(arr)
    last_f = arr[0]
    last_g = 0
    
    for i in range(1, n):
        # we take current
        temp = last_f
        last_f = max(last_f, last_g + arr[i])
        last_g = temp
    
    return last_f
    
arr = [3, 1, 1, 6, 8, 5, 1, 1]
print(main(arr))
    
    
    