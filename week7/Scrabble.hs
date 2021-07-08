{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Exercise 3

module Scrabble where

newtype Score = Score Int
  deriving (Eq, Ord, Num, Show)

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = 0

score :: Char -> Score
score c | elem c "AEILNORSTUaeilnorstu" = 1
        | elem c "DGdg"                 = 2
        | elem c "BCMPbcmp"             = 3
        | elem c "FHVWYfhvwy"           = 4
        | elem c "Kk"                   = 5
        | elem c "JXjx"                 = 8
        | elem c "QZqz"                 = 10
        | otherwise                     = 0

scoreString :: String -> Score
scoreString s = mconcat $ map score s

getScore :: Score -> Int
getScore (Score i) = i


