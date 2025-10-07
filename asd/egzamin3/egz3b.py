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
  sol = -1
  
  for i in range(n - 1):
    if T[i] in k_set:
      cnt = 1
    else:
      cnt = 0
    for j in range(i + 1, n):
      if T[j] in k_set:
        cnt += 1
        if cnt == 3:
          # we go back by 1 tile and cmp to solution
          sol = max(sol, j - i)
    if cnt < 3:
      sol = max(sol, j - i + 1)
  
  return sol
       

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( kunlucky, all_tests = False )
#print(get_k_set(3, 19))
