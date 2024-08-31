# Jan Kaczmarski
# Time complexity: O(nm)

from zad8testy import runtests

def parking(X,Y):
  dp = [[float('inf') for __ in range(len(Y))] for _ in range(len(X))]
  ctp = [[float('inf') for __ in range(len(Y))] for _ in range(len(X))]
  # we cache minimum values for previous j's
  ctp[0][0] = abs(Y[0] - X[0])
  
  # edge values for recursion
  for j in range(len(Y) ):
    if j > 0:
      ctp[0][j] = min(ctp[0][j - 1], abs(Y[j] - X[0]))
    dp[0][j] = abs(Y[j] - X[0])
  
  def f(i, j):
    nonlocal dp
    
    if dp[i][j] != float('inf'):
      return dp[i][j]

    cost_to_prev = float('inf')
    # cost to prev == cost_to_prev(min(ctp(f(i, j-1)), f(i, j-1))
    # j will never be 0, bcs of task constraints
    ctp[i][j] = min(ctp[i][j-1], f(i - 1, j-1))
      
    dp[i][j] = ctp[i][j] + abs(Y[j] - X[i])
    
    return dp[i][j]
  
  # print the result
  res = float('inf')
  for j in range(len(X) - 1, len(Y)):
    tmp = res
    res = min(res, f(len(X) - 1, j))
    # this can shorten times a bit, bcs if we meet 
    # value bigger then the current smallest we will just keep
    # getting bigger scores
    if tmp == res:
      return res
    
    
  return res
    

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( parking, all_tests = True )
