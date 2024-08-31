"""
To zadanie to jest po prostu LIS, tylko ze liczbt nie sa naturalne
tylko to przedzialy - pamietaj ze trzeba wymyslec dobra funkcje
"""

def sol(arr):
    n = len(arr)
    # dlugosc najwiekszej wiezy na i-tym elemencie
    dp = [1 for _ in range(n)]
    
    for i in range(1, n):
        for j in range(0, i):
            if arr[j][0] <= arr[i][0] and arr[j][1] >= arr[i][1]:
                dp[i] = max(dp[j] + 1, dp[i])
    
    # szukamy ile trzeba usunac, dlatego dlugosc listy - dlugosc naszej wiezy
    # to liczba ktora trzeba usunac
    return n - max(dp)


