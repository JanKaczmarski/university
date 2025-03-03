sgn :: Int -> Int
sgn n = if n < 0
    then -1
    else if n == 0
        then 0
        else 1

absInt :: Int -> Int
absInt x = if x < 0
    then (-x)
    else x

min2Int :: (Int, Int) -> Int
min2Int (x, y) = if x > y
    then y
    else x

min3Int :: (Int, Int, Int) -> Int
min3Int (x, y, z) = if (x <= y) && (x <= z)
    then x
    else if (y <= x) && (y <= z)
        then y
        else z

toLower :: Char -> Char
toLower c = toEnum(fromEnum c + 32)

toUpper :: Char -> Char
toUpper c = toEnum(fromEnum c - 32)

