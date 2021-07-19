{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving Show

cmp :: (DieValue, DieValue) -> Battlefield -> Battlefield
cmp (x, y) (Battlefield a d) | x > y     = Battlefield a (d - 1)
                             | otherwise = Battlefield (a - 1) d

helper :: Battlefield -> [DieValue] -> [DieValue] -> Battlefield
helper b av dv = foldr cmp b v 
  where v = zip av (dv ++ repeat 0)

battle :: Battlefield -> Rand StdGen Battlefield
battle b = helper b <$> replicateM (min 3 (attackers b - 1)) die
                    <*> replicateM (min 2 (defenders b)) die

invade :: Battlefield -> Rand StdGen Battlefield
invade b = battle b >>=
  \b'@(Battlefield a d) -> if (a < 2 || d <= 0) then return b' else battle b'

successProb :: Battlefield -> Rand StdGen Double
successProb b = (/ 1000) <$>
  foldr (\(Battlefield a d) x -> if (d <= 0) then (x + 1) else x) 0 <$>
    replicateM 1000 (invade b)

exactSuccessProb :: Battlefield -> Double
exactSuccessProb (Battlefield a d)
  | d <= 0 = 1
  | a < 2 = 0
  | otherwise = undefined 
