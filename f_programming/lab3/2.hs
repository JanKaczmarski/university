sumSqr' :: (Num a) => [a] -> a
sumSqr' [] = 0
sumSqr' (x : xs) = x ^ 2 + sumSqr' xs

sumWith :: (Num a) => (a -> a) -> [a] -> a
sumWith f [] = 0
sumWith f (x : xs) = f x + sumWith f xs

sum = sumWith (\x -> x)

sumSqr = sumWith (\x -> x ^ 2)

sumCube = sumWith (\x -> x ^ 3)

sumAbs = sumWith (\x -> if x < 0 then -x else x)
