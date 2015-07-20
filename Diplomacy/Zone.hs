{-|
Module      : Diplomacy.Zone
Description : ProvinceTarget with different Eq, Ord instances.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Diplomacy.Zone (

    Zone(..)

  , zoneProvinceTarget

  ) where

import Diplomacy.Province

-- | A ProvinceTarget in which coasts of the same Province are equal.
--   This notion is useful because the rules of Diplomacy state that each
--   Zone is occupied by at most one unit, i.e. there cannot be a unit at
--   two coasts of the same Province.
newtype Zone = Zone ProvinceTarget

deriving instance Show Zone

instance Eq Zone where
    Zone x == Zone y = case (x, y) of
        (Normal p1, Normal p2) -> p1 == p2
        (Special c1, Special c2) -> pcProvince c1 == pcProvince c2
        (Normal p, Special c) -> p == pcProvince c
        (Special c, Normal p) -> p == pcProvince c

instance Ord Zone where
    Zone x `compare` Zone y = case (x, y) of
        (Normal p1, Normal p2) -> p1 `compare` p2
        (Special c1, Special c2) -> pcProvince c1 `compare` pcProvince c2
        (Normal p, Special c) -> p `compare` pcProvince c
        (Special c, Normal p) -> pcProvince c `compare` p

zoneProvinceTarget :: Zone -> ProvinceTarget
zoneProvinceTarget (Zone pt) = pt
