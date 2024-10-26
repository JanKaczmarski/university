import sys
# How to categorize if point is smaller (less angle)
# Bigger if bigger angle
# 3 points -> base_point, x axis -> (base_point[0] + 1, base_point[1]), i-th point = value of 

# punkt a i b -> punkt bazowy q
# jesli a jest > b to det(q, a, b) jest > 0

def mat_det_3x3(a, b, c):
    """
    Obliczanie wyznacznika macierzy 3x3 bez użycia funkcji bibliotecznych
    :param a: krotka współrzędnych (x, y) pierwszego punktu tworzącego naszą prostą
    :param b: krotka współrzędnych (x, y) drugiego punktu tworzącego naszą prostą
    :param c: krotka współrzędnych (x, y) punktu, którego położenie względem prostej chcemy znaleźć
    :return: wartość wyznacznika macierzy
    """
    return (b[0] - a[0])*(c[1]-b[1]) - (b[1]-a[1])*(c[0]-b[0])


def partition(A, p, r, base):
    x = A[r]
    i = p - 1
    for j in range(p, r):
        if mat_det_3x3(base, A[j], x) > 0:
            i += 1
            A[i], A[j] = A[j], A[i]

    A[i + 1], A[r] = A[r], A[i + 1]
    return i + 1


def trig_sort(A, p, r, base):
    while p < r:
        q = partition(A, p, r, base)
        trig_sort(A, p, q-1, base)
        p = q + 1


def get_lowest_point(points):
    if len(points) == 0:
        print("points array is empty")
        sys.exit(1)
    sol = [float('inf'), float('inf'), float('inf')]
    for i in range(len(points)):
        if points[i][1] < sol[1]:
            sol = [points[i][0], points[i][1], i]
        elif points[i][1] == sol[1] and points[i][0] < sol[0]:
            sol = [points[i][0], points[i][1], i]
    return ((sol[0], sol[1]), sol[2])


def eliminate_suppressing_points(arr, base):
    points = []
    i = 0
    while i < len(arr) - 1:
        j = i + 1
        sol = arr[i]
        while j < len(arr) and arr[i][1] == arr[j][1]: # this should be equal on angle that they are creating with base
            # if j is further away from base than current sol
            if abs((arr[j][0] - base[0])**2 + (arr[j][1] - base[1])**2) > abs((sol[0] - base[0])**2 + (sol[1] - base[1])**2):
                sol = arr[j]
            j += 1
        points.append(sol)
        i = j
    return points


def graham_algorithm(points):
    s, id = get_lowest_point(points)
    # O(n) -> maybe some way to change it in get_lowest_point func?
    # for ex. create new array at the beggining?
    del points[id]
    trig_sort(points, 0, len(points) - 1, s)
    points = eliminate_suppressing_points(points, s)
    n = len(points)
    s = [s, points[0], points[1]]
    
    for i in range(2, n):
        while len(s)>2 and mat_det_3x3(s[-2], s[-1], points[i]) > -10**(-24):
            s.pop()
        s.append(points[i])
    return s


A = [(1,3), (0,0),(1,1),(1,2), (2,2) ,(1,1),(-1,2)] 
print(graham_algorithm(A))

