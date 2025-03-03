# Jan Kaczmarski
# Wyznaczyc wszystkie liczby k-pechowe z przedzialu {1 ... n}
# Oznaczyc gdzie w ciagu wystepuja liczby k-pechowe (sprawdzamy czy liczba jest w zbiorze liczb k-pechowych)
# i jesli jest to zapisujemy do tablice `invalid`. zlozonosc czasowa sprawdzenia to O(1)
# wyznaczmamy element gdzie 2 liczby k-pechowe sa najbardziej odlegle i bierzemy wynik 
#
# Nie widzę jednego edge case, moje rozumowanie to:
# bierzemy najwiekszy przedzial ktory zawiera 2 k-pechowe liczby. Nie rozwazamy przedzialow z 1 k-pechowa liczba
# bo taki przedzial zawsze mozna poszerzyc o 1 element, np. dodatkową liczbę k-pechową
# Załoenie algorytmu wydaje się poprawne nie widze jednak tego 1 przypadku 
# Zlozoność Czasowa O(n * m), gdzie m to liczba k-pechowych liczba w tablicy T

from egz3btesty import runtests

# funkcja zapisuje do zbioru wszytkie k-pechowe
# liczby z prezedzialu {1...n}
def get_k_set(k, n):
  k_set = set()
  k_set.add(k)
  cur = k
  i = 2
  while cur < n:
    cur = cur + (cur % (i - 1) ) + 7
    if cur <= n: k_set.add(cur)
  
  return k_set

def kunlucky(T, k):
  n = len(T)
  # wyznaczamy k-pechowe liczby z przedzialu {1...n}
  k_set = get_k_set(k, n)
  invalid = []
  
  # zapisujemy indeksy k-pechowych liczb z tablicy T w talibcy invalid
  for i in range(n):
    if T[i] in k_set:
      invalid.append(i)

  if len(invalid) <= 2:
    return n
  
  # sprawdzamy czy rozwiązaniem jest ciag od indeksu 0 do indeksu invalid[2]
  sol = invalid[2]
  
  # uzywam metody slidigin-window i sprawdzam czy wynik zmienia sie na lepsze
  SET_LEN = 3
  for i in range(len(invalid)):
    # ustawiamy prawy kraniec okna
    j = i + SET_LEN
    # jesli wyszlismy poza tablice invalid
    if j >= len(invalid):
      sol = max(sol, n - invalid[i] - 1)
      break
    else:
      # jesli mamy lepszy wynik to zmien solution
      sol = max(invalid[j] - invalid[i], sol)
      
  return sol

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( kunlucky, all_tests = True )

