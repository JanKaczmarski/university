-- Definicja funckji Sections
fiveToPower_ :: Integer -> Integer
fiveToPower_ = (5 ^)

_ToPower5 :: (Num a) => a -> a
_ToPower5 = (^ 5)

subtrNFrom5 :: (Num a) => a -> a
subtrNFrom5 = (-) 5 -- the same (5 -)
