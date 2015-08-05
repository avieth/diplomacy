{-|
Module      : Diplomacy.Turn
Description : 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Diplomacy.Turn (

    Turn
  , firstTurn
  , nextTurn
  , prevTurn
  , turnToInt
  , turnFromInt

  ) where

import Data.TypeNat.Nat

newtype Turn = Turn Nat

deriving instance Eq Turn
deriving instance Ord Turn

instance Show Turn where
    show = show . turnToInt

firstTurn = Turn Z

nextTurn :: Turn -> Turn
nextTurn (Turn n) = Turn (S n)

prevTurn :: Turn -> Maybe Turn
prevTurn (Turn Z) = Nothing
prevTurn (Turn (S n)) = Just (Turn n)

turnToInt :: Turn -> Int
turnToInt (Turn Z) = 0
turnToInt (Turn (S n)) = 1 + turnToInt (Turn n)

turnFromInt :: Int -> Maybe Turn
turnFromInt i | i < 0 = Nothing
              | i == 0 = Just firstTurn
              | otherwise = fmap nextTurn (turnFromInt (i-1))
