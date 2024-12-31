import matplotlib.pyplot as plt
import random


GEN_POINT_NUMBER = 64
QT_NODE_CAPACITY = 4

# Simple coordinate object to represent points and vectors
class XY:
    def __init__(self, x: float, y: float):
        self.x = x
        self.y = y

 
# Axis-aligned bounding box with half dimension and center
class AABB:
    def __init__(self, center: XY, half_width: float, half_height: float):
        """
        Initialize an axis-aligned bounding box (AABB).
        
        :param center: The center point of the rectangle.
        :param width: The width of the rectangle.
        :param height: The height of the rectangle.
        """
        self.center = center
        self.half_width = half_width
        self.half_height = half_height

    def contains_point(self, point: XY) -> bool:
        """
        Check if a point is within the bounds of the rectangle.
        
        :param point: The point to check.
        :return: True if the point is within the bounds, otherwise False.
        """
        x_valid = self.center.x - self.half_width <= point.x <= self.center.x + self.half_width
        y_valid = self.center.y - self.half_height <= point.y <= self.center.y + self.half_height
        
        return x_valid and y_valid

    def intersects_AABB(self, other: 'AABB') -> bool:
        """
        Check if this AABB intersects with another AABB. Two AABBs intersect
        if their projections on both axes overlap.
        
        :param other: The other AABB to check for intersection.
        :return: True if the AABBs intersect, otherwise False.
        """
        # Calculate the distance between centers
        dx = abs(other.center.x - self.center.x)
        dy = abs(other.center.y - self.center.y)
        
        # Check overlap on x-axis and y-axis
        overlap_x = dx <= (self.half_width + other.half_width)
        overlap_y = dy <= (self.half_height + other.half_height)
        
        # If both axes overlap, the rectangles intersect
        return overlap_x and overlap_y


# QuadTree class
# This class represents both one quad tree and the node where it is rooted.
class QuadTree:
    def __init__(self, boundary: AABB, capacity: int = QT_NODE_CAPACITY):
        # constant, how many elements can be stored in node
        self.qt_node_capacity = capacity

        # represent boundary of this quad tree
        self.boundary = boundary

        # XY points that this quadTree holds, len(points) <= self.qt_node_capacity
        self.points = []
        
        # Childs of this QuadTree Node, they are of type QuadTree
        self.north_west = None
        self.north_east = None
        self.south_west = None
        self.south_east = None
 
    
    def insert(self, point: XY) -> None:
        contains_point = self.boundary.contains_point(point)

        if not contains_point:
            #print(self.boundary.center.x, self.boundary.center.y, self.boundary.half_width, self.boundary.half_height, " not contains point: ", point.x, point.y)
            return

        # We hvaen't created children yet and can still put points inside a box
        if self.north_west is None and len(self.points) < self.qt_node_capacity:
            #print("Inserted", point.x, point.y, "into square: ", self.boundary.center.x, self.boundary.center.y)
            self.points.append(point)
        # if in point is in range, but we have too much inside current node
        else:
            if self.north_west is None:
                self.subdivide()

            self.north_west.insert(point)
            self.north_east.insert(point)
            self.south_west.insert(point)
            self.south_east.insert(point)
 
    def subdivide(self) -> None:
        """
        Create 4 children that fully divide this quad into four quads of equal area
        """
        q_width = self.boundary.half_width / 2
        q_height = self.boundary.half_height / 2

        # Create child AABBs
        self.north_west = QuadTree(
            AABB(XY(self.boundary.center.x - q_width, self.boundary.center.y - q_height), q_width, q_height)
        )
        self.north_east = QuadTree(
            AABB(XY(self.boundary.center.x + q_width, self.boundary.center.y - q_height), q_width, q_height)
        )
        self.south_west = QuadTree(
            AABB(XY(self.boundary.center.x - q_width, self.boundary.center.y + q_height), q_width, q_height)
        )
        self.south_east = QuadTree(
            AABB(XY(self.boundary.center.x + q_width, self.boundary.center.y + q_height), q_width, q_height)
        )

        #print(f"SUBDIVIDE_ID: {id}, parent: {self}")
        # Split all points of current quadtree to all of its children
        for point in self.points:

            self.north_west.insert(point)
            self.north_east.insert(point)
            self.south_west.insert(point)
            self.south_east.insert(point)
                
        # TODO: jk: check if there should be .clear instead of new list assignment
        self.points = []
        
    def queryRange(self, box: AABB) -> list[XY]:
        """
        Find all points that appear within a box
        """
        # Using set will handle cases when point lies on AABB border. In list we would classify it as 2 different points
        points_in_range = []

        # if we ranges don't intersect we don't have a match and return empty list
        if not self.boundary.intersects_AABB(box):
            return points_in_range

        for point in self.points:
            if box.contains_point(point):
                points_in_range.append(point)

        # We don't have any child nodes, then return        
        if self.north_west is None:
            return points_in_range

        # add all points from children that are within range
        for child in [self.north_west, self.north_east, self.south_west, self.south_east]:
            for point in child.queryRange(box):
                points_in_range.append(point)

        return points_in_range


# Random points generator
def generate_random_points(num_points, range_min, range_max):
    return [XY(random.uniform(range_min, range_max), random.uniform(range_min, range_max)) for _ in range(num_points)]


def get_points(node: QuadTree, points):
    for p in node.points:
        #print(p.x, p.y)
        points.append(p)
    for child in [node.north_west, node.north_east, node.south_west, node.south_east]:
        if child:
            get_points(child, points)

def main():
    qtree = QuadTree(
        AABB(XY(32, 32), 32, 32)
    )

    # Generate 100 random points
    random_points_upper_right = generate_random_points(GEN_POINT_NUMBER - 15, 0, 24)
    random_points = generate_random_points(15, 25, 64)

    # Plot these points
    plt.figure(figsize=(8, 8))
    plt.title("Randomly Generated Points (Range 0 to 50)")
    plt.xlabel("X")
    plt.ylabel("Y")
    plt.grid(True)
    
    x_cord = []
    y_cord = []
    
    for point in random_points:
        qtree.insert(point)
        x_cord.append(point.x)
        y_cord.append(point.y)
    for point in random_points_upper_right:
        qtree.insert(point)
        x_cord.append(point.x)
        y_cord.append(point.y)
        
    q_range = AABB(XY(8, 8), 12, 8)
    result = qtree.queryRange(q_range)
    
    print(f"Range - center: ({q_range.center.x}, {q_range.center.y}), HALF_WIDTH={q_range.half_width * 2}, HALF_WIDTH={q_range.half_height * 2}")
    for p in result:
        print(f"X: {p.x}, Y: {p.y}")
    
    print(len(result))
    pts = []
    
    get_points(qtree, pts)
    
    print(len(pts))
    
    
    plt.scatter(x_cord, y_cord, c='blue', marker='o')
    plt.show()



if __name__ == '__main__':
    main()
