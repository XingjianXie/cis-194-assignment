{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module JoinList where

import Sized
import Scrabble
import Buffer

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Exercise 1

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
a +++ b = Append (tag a <> tag b) a b

-- Exercise 2

s :: (Sized b, Monoid b) => JoinList b a -> Int
s = getSize . size . tag

indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a

indexJ 0 (Single _ a) = Just a
indexJ x (Append _ a b) | x < s a             = indexJ x a
                        | x >= (s a + s b)    = Nothing
                        | otherwise           = indexJ (x - s a) b
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a

dropJ 1 (Single _ _) = Empty
dropJ x (Append _ a b) | x < s a          = (dropJ x a) +++ b
                       | x >= (s a + s b) = Empty
                       | otherwise        = dropJ (x - s a) b
dropJ _ l = l

takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a

takeJ 0 _ = Empty
takeJ _ Empty = Empty
takeJ _ si@(Single _ _) = si
takeJ x ap@(Append _ a b) | x <= s a         = takeJ x a
                          | x >= (s a + s b) = ap
                          | otherwise        = a +++ takeJ (x - s a) b 

-- Exercise 4

replaceJ :: (Sized b, Monoid b) =>
            Int -> b -> a -> JoinList b a -> JoinList b a
replaceJ _ _ _ Empty = Empty
replaceJ 0 m l (Single _ _) = Single m l
replaceJ x m l ap@(Append _ a b) | x < s a          = replaceJ x m l a +++ b
                                 | x >= (s a + s b) = ap
                                 | otherwise        = a +++ replaceJ (x - s a) m l b 
replaceJ _ _ _ x = x

scoreLine :: String -> JoinList Score String
scoreLine x = Single (scoreString x) x

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ str) = str
  toString (Append _ a b) = toString a ++ "\n" ++ toString b

  fromString str = buildTree (lines str)
                   where buildTree []     = Empty
                         buildTree (x:[]) = Single (scoreString x, 1) x
                         buildTree list   = buildTree a +++ buildTree b
                                            where (a, b) = splitAt (length list `div` 2) list

  line = indexJ

  replaceLine x l b = replaceJ x m l b where m = (scoreString l, 1)

  numLines = getSize . snd . tag

  value = getScore . fst . tag
