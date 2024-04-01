"""
log(n) different possible values
n - numbers from range {0,..,log(n)} !! real numbers


1. First aproach
uniq = [] # we insert our values to this array
# if we want to insert we search for values using bin_search
# we insert 
time complexity of this part = O( log^2(n) )

cnt = [0] * len(uniq)

ex.
T = [3.9, 5.4 , 2.2, 5.4, 3.9, 9.6, 1.4, 2.2]
-> uniq = [1.4, 2.2, 3.9, 5.4, 9.6]
so we have labeled our values 
now we count values and store in counter
cnt = [1, 2, 2, 2, 1] # idx adheres to uniq 

2. Second aproach - quicker_sort
# we sort array using == part in main array
as so [1.4,2.2,5.4,5.4,9.6]
and we sort using quickersort parts that are smaller and bigger
so quickers([1.4,2.2]), quickers([9.6])
"""
