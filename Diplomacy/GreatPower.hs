{-|
Module      : Diplomacy.GreatPower
Description : 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Diplomacy.GreatPower (

    GreatPower(..)

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
