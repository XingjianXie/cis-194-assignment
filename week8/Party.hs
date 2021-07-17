{-# OPTIONS_GHC -Wall #-}

module Party where

import Employee
import Data.Tree
import System.IO
import Data.List (sort)

-- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons e@Emp{empFun = f'} (GL l f) = GL (e:l) (f' + f)

instance Semigroup GuestList where
  (<>) (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f Node{rootLabel = l, subForest = s} =
  f l (map (treeFold f) s)

-- Exercise 3

moreFun' :: (GuestList, GuestList) -> GuestList
moreFun' = uncurry moreFun

nextLevel :: Employee -> [(GuestList, GuestList)]
             -> (GuestList, GuestList)
nextLevel e l = (glCons e $ mconcat lb, mconcat lm)
  where (_, lb) = unzip l; lm = map moreFun' l

-- Exercise 4

maxFun :: Tree Employee -> GuestList
maxFun = moreFun' . treeFold nextLevel

-- Exercise 5

helper :: String -> String

helper c = "Total fun: " ++ show x ++ "\n" ++ unlines (sort ll)
  where GL l x = maxFun $ read c; ll = map empName l

main :: IO ()

main = do
  inh <- openFile "company.txt" ReadMode
  content <- hGetContents inh
  putStr $ helper content
  hClose inh
