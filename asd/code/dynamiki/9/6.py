"""
Mamy tablice Nominalow i kwote ktora chcemy wydac
najmniejsza ilosc monet jaka mozemy wydac dana kwote
np. dla 15 i monet 1, 5, 8 wynik to 5, 5, 5
"""

# ogolnie proste 

def monety(nom, k):
    # za pomoca ilu monet mozemy wydac kwote i to dp[i]
    dp = [float('inf') for _ in range(k + 1)]
    dp[0] = 0
    
    for i in range(1, k + 1):
        # jesli nominal mniejszy niz cel
        for x in nom:
            if i - x > 0:
                # albo aktualny wynik, albo bierzemy monete
                dp[i] = min(dp[i], dp[i - x] + 1)
    
    return dp[k]

