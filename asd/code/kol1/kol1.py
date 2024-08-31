# Jan Kaczmarski
# dla kazdego elementu tablicy zliczamy ile 
# elementow przed nim jest od niego mniejszych
# do tego potrzebujemy 2 petli, jesli ranga elementu jest wieksza
# niz dotychczasowa maksymalna zmien wartosc sol na nowa
# zlozonosc czasowa: O(n^2)
# zlozonosc pamieciowa: O(1)

from kol1testy import runtests

def maxrank(T):
    n = len(T)
    # zmienna przechowujaca wynik
    sol = 0
    # dla kazdego elementu sprawdzamy jaka jest jego ranga
    # bez T[0], bo i tak ranga T[0] = 0
    for i in range(1, n):
        cnt = 0
        # wszystkie elementy do aktualnego
        for j in range(i):
            # jesli wiekszy to zwieksz aktualna range o 1
            if T[j] < T[i]:
                cnt += 1
        # aktualizacja sol
        sol = max(cnt, sol)
    
    return sol


# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( maxrank, all_tests = True )
