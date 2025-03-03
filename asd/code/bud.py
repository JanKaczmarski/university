"""
Before reading notes:
1. In normal solution there is no need for this much comments, you should explain only solution 
specfic decisions, for example in normal soltion I would leave probably only main idea behind
solution and how I represent airplane landing spot eg. ((start_row, start_col), (end_row, end_col))

2. I wrote this in English for you to learn it a bit and get a grip of how it works in big-tech companies
3. If you have hard time understanding my comments you can write to me on private
4. Time complexity: O(n^4) -> if a lot of 'X' in array
4. Space complexity: O(1) -> we don't allocate any excess memory
5. Max memory used: 4 * 1500 ** 2 = 9MB (4 bytes for int and max array is 1500x1500) 

If I forgot sth or you have questions write to me
"""


def can_place(line, air):
    """
    Check if we can place airplane landing spots top-bottom and left-right
    """

    start, end = line
    n = len(air)
    # if we move left to right
    if start[0] == end[0]:
        # if we moved out of bounds
        if end[1] >= n: return False
        # check if all tiles are '.'
        for i in range(start[1], end[1]):
            if air[start[0]][i] == 'X': return False
    # if we move top to bottom
    else:
        if end[0] >= n: return False
        for i in range(start[0], end[0]):
            if air[i][start[1]] == 'X': return False
    
    return True


# how airplane landing line is represented in this solution
# ((start_row, start_col), (end_row, end_col))

# this part is coppied from Stack Overflow
# But I like that maths can be applied to programming like this 
# btw, this is conclustion after transformations from linear functions theory 
def ccw(A,B,C):
    return (C[1]-A[1]) * (B[0]-A[0]) > (B[1]-A[1]) * (C[0]-A[0])


# Return true if line segments AB and CD intersect
def intersect(A,B,C,D):
    # if two same segments return that they intersect -> bcs of task requirements
    if A == C and B == D: return True
    return ccw(A,C,D) != ccw(B,C,D) and ccw(A,B,C) != ccw(A,B,D)


# main funciton that calculates solution
def sol(n, m, air):
    # we'll use greedy algorithm -> try to place biggest possible airplane landing spots
    # if we can't place big enough we go with smaller one 
    for cur_len in range(n, -1, -1):
        # store all possible spots to place airplaen landing spots in poss_lines
        poss_lines = []
        for row in range(n):
            for col in range(n):
                # can we place line starting at (row, col) vertically and horizontaly
                if can_place( ((row, col), (row, col + cur_len)), air): 
                    poss_lines.append(((row, col), (row, col + cur_len)))
                if can_place( ((row, col), (row + cur_len, col)), air):
                    poss_lines.append( ((row, col), (row + cur_len, col)))
        # out of all possible airplane landing spots check if we can place both at once
        for line in poss_lines:
            for sline in poss_lines:
                if not intersect(line[0], line[1], sline[0], sline[1]): return cur_len, line, sline

    # we couldn't place any airplane landing spots so we return it's impossible
    # -> ex. if we have airplane of all 'X'
    return 0


air = [
    ['.','X','.','.','.'],
    ['.','X','X','X','X'],
    ['X','X','.','.','.'],
    ['.','.','.','.','.'],
    ['.','X','.','X','.']
]


# Solution only for m == 2
# You can modify this solution to accept solutions where m == 1
# Of course you can build your own solution
print(sol(5, 2, air))

