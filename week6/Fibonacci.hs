{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2

fib' :: Integer -> Integer -> [Integer]
fib' a b = (a + b):(fib' (b) (a + b))

fibs2 :: [Integer]
fibs2 = 0:1:fib' 0 1

-- Exercise 3

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons s xs) = s:streamToList xs

instance Show a => Show (Stream a) where
  show s = show $ take 20 (streamToList s)

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat s = Cons s $ streamRepeat s

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons s xs) = Cons (f s) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = Cons s $ streamFromSeed f (f s)

-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons s xs) ys
  = Cons s (interleaveStreams ys xs)

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (+1) ruler)

-- Exercise 6

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

amplify :: Stream Integer -> Integer -> Stream Integer
amplify si i = streamMap (*i) si

contract :: Stream Integer -> Integer -> Stream Integer
contract si i = streamMap (`div` i) si

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate (Cons s xs) = Cons (-s) (negate xs)
  (+) (Cons s xs) (Cons y ys) = Cons (s + y) $ (+) xs ys
  (*) (Cons a0 a') b@(Cons b0 b') = Cons (a0 * b0)
                                         (amplify b' a0 + a' * b)

instance Fractional (Stream Integer) where
  (/) a@(Cons a0 a') b@(Cons b0 b') = Cons (a0 `div` b0)
                                           (contract (a' - (a / b) * b') b0) 

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ 2)

-- Exercise 7

data Matrix = Matrix Integer Integer Integer Integer
  deriving Show

instance Num Matrix where
  (Matrix a11 a12 a21 a22) * (Matrix b11 b12 b21 b22) =
    Matrix (a11 * b11 + a12 * b21)
           (a11 * b12 + a12 * b22)
           (a21 * b11 + a22 * b21)
           (a21 * b12 + a22 * b22)

fib4 :: Integer -> Integer

fib4 0 = 0
fib4 n = let (Matrix _ a _ _) = (Matrix 1 1 1 0) ^ n  in a

