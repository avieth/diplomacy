{-|
Module      : Diplomacy.Occupation
Description : Definition of Zone/ProvinceTarget occupation.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Diplomacy.Occupation (

    Occupation

  , emptyOccupation
  , occupy
  , occupier
  , occupies
  , unitOccupies
  , occupied

  ) where

import qualified Data.Map as M
import Data.Maybe (isJust)
import Diplomacy.Aligned
import Diplomacy.Unit
import Diplomacy.Province
import Diplomacy.Zone

-- | Each Zone is occupied by at most one Aligned Unit, but the functions on
--   Occupation work with ProvinceTarget; the use of Zone as a key here is just
--   to guarantee that we don't have, for instance, units on both of Spain's
--   coasts simultaneously.
type Occupation = M.Map Zone (Aligned Unit)

emptyOccupation :: Occupation
emptyOccupation = M.empty

occupy :: ProvinceTarget -> Maybe (Aligned Unit) -> Occupation -> Occupation
occupy pt maunit = M.alter (const maunit) (Zone pt)

occupier :: ProvinceTarget -> Occupation -> Maybe (Aligned Unit)
occupier pt = M.lookup (Zone pt)

occupies :: Aligned Unit -> ProvinceTarget -> Occupation -> Bool
occupies aunit pt = (==) (Just aunit) . M.lookup (Zone pt)

unitOccupies :: Unit -> ProvinceTarget -> Occupation -> Bool
unitOccupies unit pt = (==) (Just unit) . fmap alignedThing . M.lookup (Zone pt)

occupied :: ProvinceTarget -> Occupation -> Bool
occupied pt = isJust . M.lookup (Zone pt)
