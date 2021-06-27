module Golf where
import Data.List

-- Exercise 1
skip :: [a] -> Int -> [a]
-- zip [1..length l] l : Form pair of index and value
-- ((==0).(`mod` x).fst) : Get the index and mod by x and check if result == 0
-- map snd ... : Get the value of each pairs
skip l x = map snd (filter ((==0).(`mod` x).fst) (zip [1..length l] l))

skips :: [a] -> [[a]]
-- Currying the function skip by passing the first argument
-- Then we can use map to create skip from each index
skips l = map (skip l) [1..length l]

-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:rest)
-- When the middle one is greater, we add it to list
-- Notice that we pass b:c:rest in the next call to check if c could be maxima
-- This could guarantee when have checked every possible maxima
  | (b > a && b > c) = b:localMaxima (b:c:rest)
  | otherwise = localMaxima (b:c:rest)
localMaxima _ = []

-- Exercise 3
point :: Bool -> Char
-- Helper to convert Bool (if a point should be here) to the actual character
point True = '*'
point False = ' '

counts :: [Integer] -> [Integer]
-- Count the occurence of 0 to 9 in the list to form new list
counts l = map (\x -> (genericLength.filter (==x)) l) [0..9]

layer :: [Integer] -> Integer -> String
-- For each layer, whether the point should be shown is judged by occurence
-- If the occurence is greater than x, the point should be shown in x th line
layer l x = map (point.(>=x)) (counts l)

histogram :: [Integer] -> String
histogram l = unlines (
    map (layer l) (reverse [1..maximum (counts l)])
 ++ ["=========="]
 ++ ["0123456789"]
 )
