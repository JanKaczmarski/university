from egzP8atesty import runtests 

def reklamy ( T, S, o ):
    # posortowac T rosnaca wzgledem startu oferty
    
    # dp[i][o] -> najwiekszy zysk kiedy zaczynamy od i,
    # a nasz pierwszy dzien od ktorego mozemy wziac nowa
    # reklame to o 
    
    # dp[0][0] -> to wynik
    
    # rozwiazanie
    
    n = len(T)
    # it's easier to operate on combined array
    comb = [(T[i][0], T[i][1], S[i]) for i in range(n)]
    comb.sort()

    dp = [[None for _ in range(o)] for _ in range(n)]
    
    for i in range(o):
        if i < comb[n-1][0]:
            dp[n-1][i] = comb[n-1][2]
        else:
            dp[n-1][i] = 0
    
    def solve(com, av_day):
        # we go out of bounds
        if av_day >= o or com >= n:
            return 0

        # value already calculated
        if dp[com][av_day] is not None:
            return dp[com][av_day]
        
        dp[com][av_day] = solve(com + 1, av_day)
        # we either take com proposal or we don't, and we take max out of it
        if av_day <= comb[com][0]:
            dp[com][av_day] = max(dp[com][av_day], solve(com + 1, comb[com][1] + 1) + comb[com][2])
            
        return dp[com][av_day]
                
    return solve(0, 0)

runtests ( reklamy, all_tests=True )
# T= [(0, 3), (4, 5), (1, 4) ]
# S= [5000,3000, 15000 ]
# o = 6
# print(reklamy(T, S, o))


# czy da sie to podzielic na dwa przypadki: biore/nie biore ?