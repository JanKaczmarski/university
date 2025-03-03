"""
Jan Kaczmarski 
Rozwiązanie wykorzystuje algorytm zachłanny. Szukamy dla i-tego
elementu najlepszej mozliwej pary i jesli mozemy je polaczyc w pare
to to robimy
zlozonosc czasowa O(n^2)
Potencjalna modyfikacja to zaczęcie w innym wierzcholku niz w 0
pozwala to wygenerowac wiecej rozwiazan

"""

from egz2atesty import runtests



def wired( T ):
  n = len(T)
  visited = [False] * n
  result = 0
  for i in range(n - 1):
    if visited[i]: continue
    wire = [i, None, float('inf')]
    for j in range(i + 1, n):
      if visited[j]: 
        break 
      if (j - i) % 2 == 0:
        continue
      if 1 + abs(T[i] - T[j]) <= wire[2]:
        wire = [i, j, 1 + abs(T[i] - T[j])]
    if wire[1] is None:
      continue
    visited[wire[1]] = True
    visited[wire[0]] = True
    result += wire[2] 
  
  return result

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( wired, all_tests = True )
