"""
Wierzcholek v w grafie skierowanym nazywamy dobrym poczatkiem jesli kazdy inny wierzcholek 
w tym grafie jest osiagalny sciezka wychodzaca z tego wierzocholka. Stwierdzic czy graf 
zawiera dobry poczatek 
"""

# 1. Dobre ale nie optymalne
# dfs na caly graf startujac od konkretnego wierzcholka
# jesli przejdziemy caly graf to mamy dobry poczatek

# 2. Spojne skladowe
# Dzielimy graf na graf spojnych skladowych  G
# sortujemy graf G topologicznie
# jesli z pierwszej po posortowaniu spojnej skladowej, mozemy 
# dojsc do kazdej spojnej skladowej to mamy wynik


# 3. Czas przetworzenia