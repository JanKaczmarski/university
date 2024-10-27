prod'2 :: (Num a) => [a] -> a
length'2 :: [a] -> Int
prod'2 xs = loop 1 xs
  where
    loop acc [] = acc
    loop acc (x : xs) = loop (acc * x) xs

length'2 xs = loop 0 xs
  where
    loop acc [] = acc
    loop acc (x : xs) = loop (acc + 1) xs
