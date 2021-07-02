{-# OPTIONS_GHC -Wall #-}
import Data.List ((\\))

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

-- Exercise 2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = -1
height (Node x _ _ _) = x

insert :: a -> Tree a -> Tree a
insert element Leaf = Node 0 Leaf element Leaf
insert element (Node x left element' right)
  | height left < height right = Node x (insert element left) element' right
  | height left > height right = Node x left element' (insert element right)
  | otherwise                  = Node (1 + max (height left) (height right'))
                                      left element' right'
                                      where right' = insert element right

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

-- Exercise 3

xor :: [Bool] -> Bool
xor = foldl (\a a' -> (a && not a') || (not a && a')) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\aElement bList -> f aElement:bList) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
-- This is an simple enough logic that we just reverse the xs
-- But I believe this is not what you really want
myFoldl f base xs = foldr (\x -> (`f` x)) base (reverse xs)

-- Exercise 4

sieved :: Integer -> [Integer]
sieved x = filter (<=x) [i + j + 2 * i * j | j <- [1..x], i <- [1..j]]

rest :: Integer -> [Integer]
rest x = [1,3..x] \\ sieved x

sieveSundaram :: Integer -> [Integer]
sieveSundaram = map ((+1).(*2)) . rest
