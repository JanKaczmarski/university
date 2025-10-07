absInt n =
    case (n >= 0) of
    True -> n
    _ -> n

isItTheAnswer :: String -> Bool
isItTheAnswer s =
    case (s == "Love") of
        True -> True
        _ -> False

not' :: Bool -> Bool
not' b =
    case b of
        True -> False
        False -> True

or' :: (Bool, Bool) -> Bool
or' (x, y) =
    case (x == False && y == False) of
        True -> False
        _ -> True


