from egz3btesty import runtests

# Wyznaczyc wszystkie liczby k-pechowe z przedzialu {1 ... n}
# Oznaczyc gdzie w ciagu wystepuja liczby k-pechowe (sprawdzamy czy liczba jest w zbiorze liczb k-pechowych)
# zlozonosc czasowa sprawdzenia to O(1)
# wyznaczmamy element gdzie 2 liczby k-pechowe sa najbardziej odlegle i bierzemy wynik 

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
  k_set = get_k_set(k, n)
  invalid = []
  sol = 0
  
  for i in range(n):
    if T[i] in k_set:
      invalid.append(i)
      if len(invalid) > 3:
        sol = max(sol, invalid[-1] - invalid[-4] - 1)

  if len(invalid) <= 2:
    return n
  
  # we start at index 0 and take first two k-invalid numbers
  sol = max(sol, invalid[2])
  return sol
  SET_LEN = 3

  for i in range(len(invalid)):
    # Right border of window
    # in this scenario we take 2 k-invalid elements between i and j
    j = i + SET_LEN
    # We reach out of bounds -> there are no 2 elements between i and j
    if j >= len(invalid):
      sol = max(sol, n - invalid[i] - 1)
      break
    else:
      sol = max(invalid[j] - invalid[i], sol)
      
  return sol

T = [11, 11, 19, 9, 25, 7, 1, 7, 5, 25, 12, 8, 22, 7, 19, 7, 3, 5, 3, 25, 4, 19, 12, 23, 23]
k = 4
# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( kunlucky, all_tests = False )

