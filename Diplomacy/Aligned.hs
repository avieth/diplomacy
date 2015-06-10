{-|
Module      : Diplomacy.Aligned
Description : Align datatypes to GreatPowers
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Diplomacy.Aligned (

    Aligned
  , align
  , alignedThing
  , alignedGreatPower

  ) where

import Diplomacy.GreatPower

data Aligned t where
    Aligned :: t -> GreatPower -> Aligned t

deriving instance Eq t => Eq (Aligned t)
deriving instance Ord t => Ord (Aligned t)
deriving instance Show t => Show (Aligned t)

instance Functor Aligned where
    fmap f (Aligned x y) = Aligned (f x) y

align :: t -> GreatPower -> Aligned t
align = Aligned

alignedThing :: Aligned t -> t
alignedThing (Aligned x _) = x

alignedGreatPower :: Aligned t -> GreatPower
alignedGreatPower (Aligned _ x) = x
