module Diplomacy.Unit (

    Unit

  , army
  , fleet

  , isArmy
  , isFleet

  , Army(..)
  , Fleet(..)


  ) where

data Army = Army
  deriving (Eq, Ord, Show)

data Fleet = Fleet
  deriving (Eq, Ord, Show)

-- | TODO will have to put some location information on here I think.
data Unit = UArmy Army | UFleet Fleet
  deriving (Eq, Ord, Show)

army :: Army -> Unit
army = UArmy

fleet :: Fleet -> Unit
fleet = UFleet

isArmy :: Unit -> Bool
isArmy (UArmy _) = True
isArmy (UFleet _) = False

isFleet :: Unit -> Bool
isFleet (UFleet _) = True
isFleet (UArmy _) = False
