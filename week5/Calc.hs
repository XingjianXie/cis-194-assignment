{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import qualified StackVM
import qualified Data.Map as M

-- Exercise 1

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-- Exercise 2

evalStr :: String -> Maybe Integer
evalStr str = parseExp Lit Add Mul str >>= (Just . eval)

-- Exercise 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

-- Exercise 4

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)


newtype MinMax  = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

newtype Mod7    = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
  mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- Exercise 5

instance Expr StackVM.Program where
  lit = (:[]) . StackVM.PushI
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul

-- Exercise 6

class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
              | Var String
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars VarExprT where
  var = Var

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x _ = Just x
  add x y m = do {
    x' <- x m; y' <- y m;
    return (x' + y')
  }
  mul x y m = do {
    x' <- x m; y' <- y m;
    return (x' * y')
  }

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var x = M.lookup x

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

