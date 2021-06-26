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
  | otherwise          = let a:b:rest = list in (a*2):b:doubleEveryOther rest

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits list = sum (map (sum.toDigitsRev) list)

-- Exercise 4
validate :: Integer -> Bool
validate number = (sumDigits.doubleEveryOther.toDigits) number `mod` 10 == 0
