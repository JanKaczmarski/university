# Jan Kaczmarski
# Zlozonosc czasowa O(nm)

from zad9testy import runtests

def get_valid_moves(M, i, j):
  result = []
  # j moves in Y axis
  if j - 1 > -1 and M[i][j - 1] > M[i][j]:
    result.append((i, j-1))
  if j + 1 < len(M[0]) and M[i][j + 1] > M[i][j]:
    result.append((i, j+1))
  if i - 1 > -1 and M[i - 1][j] > M[i][j]:
    result.append((i - 1, j))
  if i + 1 < len(M) and M[i + 1][j] > M[i][j]:
    result.append((i + 1, j))    

  return result
  

def trip(M):
  m = len(M)
  n = len(M[0])
  # dp stores max value we can reach starting in i,j tile
  dp = [[1 for _ in range(n)] for __ in range(m)]
  
  def f(i, j):
    nonlocal dp
    valid_moves = get_valid_moves(M, i ,j)

    # we can no longer make moves
    if len(valid_moves) == 0:
      return dp[i][j]
  
    # we have already computed the value  
    if dp[i][j] != 1:
      return dp[i][j]
    
    for move in valid_moves:
      # we move uphill and get max value
      dp[i][j] = max(dp[i][j], f(move[0], move[1]))
    dp[i][j] += 1
    
    return dp[i][j]
  
  result = 1
  for i in range(m):
    for j in range(n):
      result = max( f(i,j), result )

  return result


# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( trip, all_tests = True )
