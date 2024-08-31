"""
Subject: 2 elems that there are no numbers between then
ex. [30,50,45] -> numbers would be 30, 45 because between 30 and 50 is 45
and theier diff is the biggest, array isn't sorted 
"""

# data: N Real numbers
# len(T) == n -> True
# digits are pair-different
"""
Approach: Using buckets
Steps:
1. create buckets -> min(T) and max(T)
A there are empty buckets
2a. Eliminate empty buckets and remember biggest gap and id of bucket that gap started on
3a. max from left_bucket and min from right
B there aren't
2b. split numbers into N buckets 
BA empty bucket
3ba. repeat 2a
BB all buckets have elem
3bb. array is sorted
4bb. we find biggest diff lineary
We have solution

Example:
T = [7, 5, 8, 12, 95]
buckets = [[7,5,8,12], [], [95]]

"""
