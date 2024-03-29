{-|
Module      : Diplomacy.GreatPower
Description : Definition of the great powers (countries).
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Diplomacy.GreatPower (

    GreatPower(..)
  , allGreatPowers

  ) where

data GreatPower where
    England :: GreatPower
    Germany :: GreatPower
    France :: GreatPower
    Italy :: GreatPower
    Austria :: GreatPower
    Russia :: GreatPower
    Turkey :: GreatPower

deriving instance Eq GreatPower
deriving instance Ord GreatPower
deriving instance Show GreatPower
deriving instance Read GreatPower
deriving instance Enum GreatPower
deriving instance Bounded GreatPower

allGreatPowers :: [GreatPower]
allGreatPowers = [minBound..maxBound]
