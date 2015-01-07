{-# LANGUAGE RoleAnnotations #-}

module Diplomacy.Board where

import Diplomacy.Unit
import Diplomacy.Province
import Diplomacy.Phase
import Diplomacy.Country
import Diplomacy.ResolvedOrder

type role Board nominal
data Board phaseType

occupies :: AlignedUnit -> ProvinceTarget -> Board phaseType -> Bool

hasDislodged :: Board Retreat -> ProvinceTarget -> Bool

isDislodged :: Board Retreat -> AlignedUnit -> ProvinceTarget -> Bool

unitOccupies :: Unit -> ProvinceTarget -> Board phaseType -> Bool

countryOccupies :: Country -> ProvinceTarget -> Board phaseType -> Bool

anyResolvedOrder :: Board Retreat -> (ResolvedOrder Typical -> Bool) -> Bool
