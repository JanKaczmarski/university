# sort  A[k] in {0,..., n^2 -1} and n = len(A)

"""
We convert number to n based numeric system
ex. we have T=[11, 4, 6, 2] -> n = 4 and number are between 0 and 4^2 - 1 = 15
so we can write each number using tuple 
for 11 we have 11 = (2,3)
and then we sort using raidx_sort

IMPORTANT: both sorting algorithms need to be stable
"""


def c_sort(T, p, m):
    """
    T = [(2,3), (11,5), (6,93), ...]    
    m -> possible max(T)
    p -> sort by which element in array
    """
    n = len(T)
    C = [0 for _ in range(m + 1)]
    for el in T:
        #print(el[p])
        C[el[p]] += 1
        #print(C)

    for i in range(1, m + 1):
        C[i] += C[i - 1]

    B = [None] * n
    #print(C)
    for i in range(n-1, -1, -1):
        x = T[i][p]
        #print(x)
        pos = C[x] - 1
        #print(pos)
        #print("*****")
        B[pos] = T[i]
        C[x] -= 1
    

    return B


def sq_sort(T):
    n = len(T)
    # convert number to tuples in n-based sytem
    T_n = [None] * n
    for i in range(n):
        # convert num to n-based system
        f_digit = (T[i]//n) % n
        s_digit = T[i] % n
        T_n[i] = (f_digit, s_digit)
    # stable sort by second digit
    T_n = c_sort(T_n, 1, n)
    # stable sort by first digit
    T_n = c_sort(T_n, 0, n)
    for i in range(n):
        # convert to decimal system
        T[i] = n*T_n[i][0] + T_n[i][1]


T = [(2, 7), (6, 7), (6, 3), (10, 9), (2, 3), (10, 5), (10, 1)]
max_f = 10
max_s = 9
n = len(T)
T = c_sort(T, 1, max_s)
T = c_sort(T, 0, max_f)
print(T)
