-- Exercise 1

fun1' :: [Integer] -> Integer
fun1' l = product (map (+(-2)) (filter even l))

next :: Integer -> Integer
-- Wholemeal programming doesn't mean that we could not use piecewise function
-- However, we should simplify code to make the "piecewise logic" independent
next x | even x    = x `div` 2
       | otherwise = x * 3 + 1

fun2' :: Integer -> Integer
fun2' x = sum (filter even (takeWhile (/=1) (iterate next x)))
