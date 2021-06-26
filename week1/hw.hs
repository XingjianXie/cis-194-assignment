{-# OPTIONS_GHC -Wall #-}

-- Exercise 1
toDigitsRev :: Integer -> [Integer]
toDigitsRev number
  | number <= 0 = []
  | otherwise   = (mod number 10):toDigitsRev (div number 10)

toDigits :: Integer -> [Integer]
toDigits number = reverse (toDigitsRev number)

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list
  | list == []         = []
  | odd (length list)  = head list:doubleEveryOther (tail list)
  | otherwise          = let a:b:rest = list in (a * 2):b:doubleEveryOther rest

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits list = sum (map (sum.toDigitsRev) list)

-- Exercise 4
validate :: Integer -> Bool
validate number = (sumDigits.doubleEveryOther.toDigits) number `mod` 10 == 0

-- Exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n == 1    = [(a, b)]
  | otherwise = hanoi (n - 1) a c b ++ (a, b):hanoi (n - 1) c b a

-- Exercise 6
k :: Integer -> Integer
k n = n - round (sqrt (fromInteger (n * 2 + 1) :: Float)) + 1

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d
  | n == 1    = [(a, b)]
  | otherwise = hanoi4     (k n) a d b c
             ++ hanoi  (n - k n) a b c
             ++ hanoi4     (k n) d b a c


