module Diplomacy.Unit (

    Unit

  , army
  , fleet

  , isArmy
  , isFleet

  , Army(..)
  , Fleet(..)

  ) where

import Diplomacy.Country

data Army = Army
  deriving (Eq, Ord, Show)

data Fleet = Fleet
  deriving (Eq, Ord, Show)

data Unit = UArmy Army | UFleet Fleet
  deriving (Eq, Ord, Show)

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
