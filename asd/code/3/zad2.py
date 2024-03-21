# struktura danych
# - insert logn
# - remove_median O(logn)

"""
K1 M K2 - kopiec1, mediana, kopiec 2

K1 - min_heap
K2 - max heap
M - int

K1 = [6,3]
M = 7
K2 = [9, 10]

ins(5)
5 < M => k1.increase_heap(5)
zmiana mediany i zmiana K2

itd
"""
