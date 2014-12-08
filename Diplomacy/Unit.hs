module Diplomacy.Unit (

    Unit

  , army
  , fleet

  , isArmy
  , isFleet

  , Army(..)
  , Fleet(..)

  , AlignedUnit
  , align
  , alignedUnit
  , alignedCountry

  ) where

import Diplomacy.Country

data Army = Army
  deriving (Eq, Ord, Show)

data Fleet = Fleet
  deriving (Eq, Ord, Show)

-- | TODO will have to put some location information on here I think.
data Unit = UArmy Army | UFleet Fleet
  deriving (Eq, Ord, Show)

data AlignedUnit = AlignedUnit Unit Country
  deriving (Eq, Ord, Show)

align :: Unit -> Country -> AlignedUnit
align = AlignedUnit

alignedCountry :: AlignedUnit -> Country
alignedCountry (AlignedUnit _ c) = c

alignedUnit :: AlignedUnit -> Unit
alignedUnit (AlignedUnit u _) = u

army :: Unit
army = UArmy Army

fleet :: Unit
fleet = UFleet Fleet

isArmy :: Unit -> Bool
isArmy (UArmy _) = True
isArmy (UFleet _) = False

isFleet :: Unit -> Bool
isFleet (UFleet _) = True
isFleet (UArmy _) = False
