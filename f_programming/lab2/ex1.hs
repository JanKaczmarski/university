add2T :: (Num a) => (a, a) -> a
add2T (x, y) = x + y

-- Currying, Partialy applied functions
-- add2C :: (Num a) => (a, a) -> a
-- add2C (x, y) = x + y

add2C :: (Num a) => a -> a -> a -> a
add2C x y z = x + y + z

-- Task 1 completed
