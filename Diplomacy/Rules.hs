module Diplomacy.Rules (

    isHomeSupplyCentre
  , canBuildHere

  , canMoveFromTo

  ) where

import Diplomacy.Board
import Diplomacy.Province
import Diplomacy.Country
import Diplomacy.Order
import Diplomacy.Unit

-- | TODO should move this one to Province module maybe?
isHomeSupplyCentre :: Country -> Province -> Bool
isHomeSupplyCentre c p = (supplyCentre p) && (maybe False ((==) c) (country p))

canBuildHere :: Board -> Country -> Province -> Bool
canBuildHere b c p = (isHomeSupplyCentre c p) && (controls b c p)

-- Let's dive in with the most straightforward program for resolving ordres:
-- the very definition of resolving orders!
-- 
-- We prefer to characterize unsuccessful orders, because the user is typically
-- interested in why an order failed, not why it succeeded.

type OrderSet = [Order]

-- | TODO sum of all possible reasons
type Reason = String

data OrderOutcome
  = Unsuccessful Reason
  | Successful
    deriving (Show)

type OrderOutcomeFunction = Order -> Board -> OrderSet -> OrderOutcome

or :: OrderOutcomeFunction -> OrderOutcomeFunction -> OrderOutcomeFunction
or f g order board orderSet = case f order board orderSet of
  Successful -> g order board orderSet
  unsuccessful -> unsuccessful

canOccupyProvince :: Unit -> Province -> Bool
canOccupyProvince unit province = case provinceType province of
  Inland -> isArmy unit
  Water -> isFleet unit
  Coastal -> True

canOccupy :: Unit -> ProvinceTarget -> Bool
canOccupy unit pt = canOccupyProvince unit (ptProvince pt)

-- | Valid moves do not correspond exactly to adjacent provinces.
--   We account here for the special cases involving coastlines.
--   See https://www.wizards.com/avalonhill/rules/diplomacy.pdf page 5
--   The Kiel/Constantinople and Sweden/Denmark clarifications are already
--   defined in the Province adjacency; we need only look after the split
--   coastlines in StPetersburg, Bulgaria, and Spain.
canMoveFromTo :: ProvinceTarget -> ProvinceTarget -> Bool
-- Easiest case first: convince yourself that a fleet on a special coastline
-- can never move to another special coastline (this is the whole point of
-- the special case!) :)
canMoveFromTo (Special _) (Special _) = False
canMoveFromTo pt0 (Special pc) = let prv0 = ptProvince pt0
  in (not (blacklist prv0 pc)) && (not (isInland prv0)) && ((pcProvince pc) `adjacent` prv0)
-- This one is definitely correct; canMoveFromTo is absolutely, certainly
-- symmetric!
canMoveFromTo (Special pc) pt0 = canMoveFromTo pt0 (Special pc)
-- The fall-through case: just use the Province adjacency definition.
canMoveFromTo pt0 pt1 = (ptProvince pt0) `adjacent` (ptProvince pt1)

-- | True iff the given province should not be considered left-adjacent to the
--   given province coast.
--   This is how that special case on page 5 is expressed.
blacklist :: Province -> ProvinceCoast -> Bool
blacklist WesternMediterranean SpainNorth = True
-- NB MidAtlanticOcean to SpainSouth is fine!
blacklist GulfOfBothnia StPetersburgNorth = True
blacklist BarentsSea StPetersburgWest = True
blacklist BlackSea BulgariaSouth = True
blacklist AegeanSea BulgariaEast = True
blacklist _ _ = False

orderValid :: Board -> Order -> Bool
orderValid board (Hold unit pt0) = occupies board unit pt0
orderValid board (Move unit pt0 pt1) =
  (canOccupy unit pt1) && (occupies board unit pt0) && (canMoveFromTo pt0 pt1)

{-
TODO
orderOutcome :: Board -> OrderSet -> OrderOutcome
orderOutcome board orderSet h@(Hold _ _) = holdOutcome board orderSet h
--orderOutcome board orderSet m@(Move _ _ _) = isMoveSuccessful board orderSet m
--orderOutcome board orderSet s@(Support _ _ _ _ _) = isSupportSuccessful board orderSet s
--orderOutcome board orderSet c@(Convoy _ _ _ _ _) = isConvoySuccessful board orderSet c

-- Start with hold. By definition, a hold is unsuccessful iff it's either
-- an invalid order or there is some other force moving into its province that
-- has more support.
holdOutcome = orderInvalid `or` notStrongestForce

moveOutcome = orderInvalid `or` 
-}
