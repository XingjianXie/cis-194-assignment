{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- Exercise 6

import Parser
import qualified Data.Map as M

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
              | Var String
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

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

