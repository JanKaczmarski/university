from kol1testy import runtests
# Jan Kaczmarski
# dla kazdego elementu tablicy zliczamy ile 
# elementow przed nim jest od niego mniejszych
# do tego potrzebujemy 2 petli, jesli ranga elementu jest wieksza
# niz dotychczasowa maksymalna zmien wartosc sol na nowa
# zlozonosc czasowa: O(n^2)
# zlozonosc pamieciowa: O(1)

def maxrank(T):
    n = len(T)
    # zmienna przechowujaca wynik
    sol = 0
    # dla kazdego elementu sprawdzamy jaka jest jego ranga
    # bez T[0], bo i tak ranga T[0] = 0
    for i in range(n-1, 0, -1):
        cnt = 0
        # wszystkie elementy do aktualnego
        if sol > i + 1:
            return sol
        for j in range(i-1, -1, -1):
            # jesli wiekszy to zwieksz aktualna range o 1
            if T[j] < T[i]:
                cnt += 1
        # aktualizacja sol
        sol = max(cnt, sol)
    
    return sol

runtests( maxrank, all_tests = True )