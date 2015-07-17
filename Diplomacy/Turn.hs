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

module Diplomacy.Turn (

    Turn
  , firstTurn
  , nextTurn
  , turnToInt

  ) where

import Data.TypeNat.Nat

newtype Turn = Turn Nat

instance Show Turn where
    show = show . turnToInt

firstTurn = Turn Z

nextTurn :: Turn -> Turn
nextTurn (Turn n) = Turn (S n)

turnToInt :: Turn -> Int
turnToInt (Turn Z) = 0
turnToInt (Turn (S n)) = 1 + turnToInt (Turn n)
