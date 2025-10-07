-- we use Int -> Int beacuse length output is type Int, so we are bound by it
-- cleaner way would be to just use :: Int -> Int
numOfPitagroasThrees :: (Num n, Eq n, Enum n, Ord n) => n -> Int
numOfPitagroasThrees n = length [(a, b, c) | a <- [1 .. n], b <- [a .. n], c <- [b .. n], a ^ 2 + b ^ 2 == c ^ 2]

isPrime :: (Integral t) => t -> Bool
isPrime n = [i | i <- [2 .. n - 1], n `mod` i == 0] == []
