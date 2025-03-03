# Jan Kaczmarski
# Elementarny algorytm rozpatrujemy wszystkie mozliwe przedzialy
# zliczamy sume tego przedzialu po usunieciu k najmniejszych ujemnych elementow
# zlozonosc czasowa O(n^3logn)

# W miejscu gdzie sumuje elementy danego przedzialu myslalem 
# nad spamietywaniem w tablicy nxn krotek (wynik, first, second)
# gdzie first to najwiekszy z najmnieszych elementow w tym przedziale
# a second to drugi najwiekszy z najniejszych w tym przedziale
# pomoglo by to zmniejszyc zlozonosc do O(n^3), ale zabraklo mi czasu

from egz1btesty import runtests

def kstrong( T, k):
  result = float('-inf')
  n = len(T)
  # dla kadzego przedzialu szukamy wyniku
  # O(n^2) - rozpatrujemy wszystkie przedzialy
  for i in range(n):
    for j in range(i, n):
      # sortujemy O(nlogn)
      # i sumujemy O(n)
      # czyli zlozonosc czasowa to O(nlogn)
      
      # mozliwe ze w naszych k najmniejszych elementach sa elementy nieujemne
      # zatem chcemy je dodac do wyniku
      tmp = 0
      curr = sorted(T[i:j+1])
      for num in curr[:k]:
        if num < 0:
          continue
        tmp += num
        
      # aktualizacja wyniku
      result = max(result, sum(curr[k:]) + tmp)  
      
  return result


# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( kstrong, all_tests = False )