from egzP3atesty import runtests
from math import inf

class Node:
  def __init__(self, wyborcy, koszt, fundusze):
    self.next = None
    self.wyborcy = wyborcy 
    self.koszt = koszt 
    self.fundusze = fundusze 
    self.x = None


def extract_data(head):
  result = []
  while head != None:
    result.append((head.wyborcy, head.koszt, head.fundusze))
    head = head.next
  return result

def wybory(T):

    result = 0
    for election in T:
      data = extract_data(election)
      n = len(data)
      funds = data[0][2]
      dp = [[float('-inf') for _ in range(funds + 1)] for _ in range(n)]
      
      # we create border values for our knapsack
      # if our funds are greater or equal to our cost
      # we add this electors to solution
      # else 0
      for j in range(data[0][1], funds + 1):
        dp[0][j] = data[0][0]
        
      for b in range(funds + 1):
        for i in range(1, n):
          dp[i][b] = dp[i - 1][b]
          
          if b >= data[i][1]:
            dp[i][b] = max(dp[i][b], dp[i-1][b - data[i][1]] + data[i][0])
            
      result += dp[n-1][funds]
    
    return result
      

runtests(wybory, all_tests = True)