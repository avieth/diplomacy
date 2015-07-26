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
  , provinceOccupier
  , occupies
  , unitOccupies
  , occupied
  , zoneOccupied
  , allSubjects

  ) where

import qualified Data.Map as M
import Data.MapUtil
import Data.Maybe (isJust)
import Diplomacy.Aligned
import Diplomacy.Unit
import Diplomacy.Province
import Diplomacy.Zone
import Diplomacy.Subject
import Diplomacy.GreatPower

-- | Each Zone is occupied by at most one Aligned Unit, but the functions on
--   Occupation work with ProvinceTarget; the use of Zone as a key here is just
--   to guarantee that we don't have, for instance, units on both of Spain's
--   coasts simultaneously.
type Occupation = M.Map Zone (Aligned Unit)

emptyOccupation :: Occupation
emptyOccupation = M.empty

occupy :: ProvinceTarget -> Maybe (Aligned Unit) -> Occupation -> Occupation
occupy pt maunit = M.alter (const maunit) (Zone pt)

-- | Must be careful with this one! We can't just lookup the Zone corresponding
--   to the ProvinceTarget; we must also check that the key matching that Zone,
--   if there is one in the map, is also ProvinceTarget-equal.
occupier :: ProvinceTarget -> Occupation -> Maybe (Aligned Unit)
occupier pt occupation = case lookupWithKey (Zone pt) occupation of
    Just (zone, value) ->
        if zoneProvinceTarget zone == pt
        then Just value
        else Nothing
    _ -> Nothing

provinceOccupier :: Province -> Occupation -> Maybe (Aligned Unit)
provinceOccupier pr occupation = case lookupWithKey (Zone (Normal pr)) occupation of
    Just (zone, value) ->
        if zoneProvinceTarget zone == Normal pr
        then Just value
        else Nothing
    _ -> Nothing

occupies :: Aligned Unit -> ProvinceTarget -> Occupation -> Bool
occupies aunit pt = (==) (Just aunit) . occupier pt

unitOccupies :: Unit -> ProvinceTarget -> Occupation -> Bool
unitOccupies unit pt = (==) (Just unit) . fmap alignedThing . occupier pt

occupied :: ProvinceTarget -> Occupation -> Bool
occupied pt = isJust . occupier pt

zoneOccupied :: Zone -> Occupation -> Bool
zoneOccupied zone = isJust . M.lookup zone

allSubjects :: Maybe GreatPower -> Occupation -> [Subject]
allSubjects maybeGreatPower = M.foldWithKey f []
  where
    f zone aunit =
        let subject = (alignedThing aunit, zoneProvinceTarget zone)
        in  if maybeGreatPower == Nothing || Just (alignedGreatPower aunit) == maybeGreatPower
            then (:) subject
            else id
