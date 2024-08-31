"""
2 tablice o dlugosci n i znalezc najdluzszy wspolny podicag
z tych dwoch tablic LCS (Longest commong subsequent)
"""

# ogolnie smiesznie 
# to jest proste ogolnie zrozumialem
# po prostu slabo tlumacza tresc zadania
def findlcs(A, B):
    n = len(A)
    m = len(B)
    dp = [[0 for __ in range(m + 1)] for _ in range(n + 1)]

    for i in range(1, n + 1):
        for j in range(1, m + 1):
            dp[i][j] = max(dp[i-1][j], dp[i][j - 1])
            if A[i - 1] == B[j - 1]:
                dp[i][j] = max(dp[i - 1][j - 1] + 1, dp[i][j])

    return dp[n][m]
