{-|
Module      : Diplomacy.Turn
Description : Definition of a turn in a game of Diplomacy.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE StandaloneDeriving #-}

module Diplomacy.Turn (

    Turn (..)
  , firstTurn
  , nextTurn
  , prevTurn
  , turnToInt
  , turnFromInt

  ) where

import Numeric.Natural

newtype Turn = Turn Natural

deriving instance Eq Turn
deriving instance Ord Turn

instance Show Turn where
    show = show . turnToInt

firstTurn = Turn 0

nextTurn :: Turn -> Turn
nextTurn (Turn n) = Turn (n + 1)

prevTurn :: Turn -> Maybe Turn
prevTurn (Turn 0) = Nothing
prevTurn (Turn n) = Just (Turn (n - 1))

turnToInt :: Turn -> Int
turnToInt (Turn n) = fromIntegral n

turnFromInt :: Int -> Maybe Turn
turnFromInt i | i < 0 = Nothing
              | otherwise = Just (Turn (fromIntegral i))
