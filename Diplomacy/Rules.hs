module Diplomacy.Rules (

    isHomeSupplyCentre
  , canBuildHere

  , unitCanMoveFromTo

  ) where

import Control.Applicative

import Diplomacy.Board
import Diplomacy.Province
import Diplomacy.Country
import Diplomacy.Order
import Diplomacy.Unit

-- | TODO should move this one to Province module maybe?
isHomeSupplyCentre :: Country -> Province -> Bool
isHomeSupplyCentre c p = (supplyCentre p) && (isHome c p)

canBuildHere :: Board -> Country -> Province -> Bool
canBuildHere b c p = (isHomeSupplyCentre c p) && (controls b c p)

-- Let's dive in with the most straightforward program for resolving orders:
-- the very definition of resolving orders!
-- 
-- We prefer to characterize unsuccessful orders, because the user is typically
-- interested in why an order failed, not why it succeeded.

unitCanOccupyProvince :: Unit -> Province -> Bool
unitCanOccupyProvince unit province = case provinceType province of
  Inland -> isArmy unit
  Water -> isFleet unit
  Coastal -> True

unitCanOccupy :: Unit -> ProvinceTarget -> Bool
unitCanOccupy unit pt = unitCanOccupyProvince unit (ptProvince pt)

-- | Valid moves do not correspond exactly to adjacent (incl. convoy) provinces.
--   We account here for the special cases involving coastlines.
--   See https://www.wizards.com/avalonhill/rules/diplomacy.pdf page 5
--   The Kiel/Constantinople and Sweden/Denmark clarifications are already
--   defined in the Province adjacency; we need only look after the split
--   coastlines in StPetersburg, Bulgaria, and Spain.
unitCanMoveFromTo :: Unit -> ProvinceTarget -> ProvinceTarget -> Bool
-- Easiest case first: convince yourself that a fleet on a special coastline
-- can never move to another special coastline (this is the whole point of
-- the special case!) :)
-- Of course, this is also false if the unit is an army; it does not make sense
-- to put an army on one of these special places, they just go on the province
-- represented by it.
unitCanMoveFromTo _ (Special _) (Special _) = False
-- NB we do not use convoyAdjacent in this case because one can NEVER convoy
-- to a special ProvinceTarget; only fleets can move there, and fleets cannot
-- move as part of a convoy.
unitCanMoveFromTo unit pt0 (Special pc) = let prv0 = ptProvince pt0
  in (not (blacklist prv0 pc)) && (not (isInland prv0)) && ((pcProvince pc) `adjacent` prv0)
-- This one is definitely correct; unitCanMoveFromTo is absolutely, certainly
-- symmetric!
unitCanMoveFromTo unit (Special pc) pt0 = unitCanMoveFromTo unit pt0 (Special pc)
-- The fall-through case: just use the Province adjacency definition, eliminating
-- a convoy-adjacent move for fleets.
unitCanMoveFromTo unit pt0 pt1 =
  if isArmy unit
  then (unitCanMoveFromToNoConvoy unit pt0 pt1) || (ptProvince pt0) `convoyAdjacent` (ptProvince pt1)
  else (unitCanMoveFromToNoConvoy unit pt0 pt1)

unitCanMoveFromToNoConvoy :: Unit -> ProvinceTarget -> ProvinceTarget -> Bool
unitCanMoveFromToNoConvoy _ (Normal prv0) (Normal prv1) = prv0 `adjacent` prv1
-- unitCanMoveFromTo will not appeal to unitCanMoveFromToNoConvoy in the
-- Special cases, so we're safe to refer to it here.
unitCanMoveFromToNoConvoy unit x y = unitCanMoveFromTo unit x y

unitCanMoveFromToWithConvoy unit (Normal prv0) (Normal prv1) =
  (isArmy unit) && (prv0 `convoyAdjacent` prv1)
unitCanMoveFromToWithConvoy _ _ _ = False

-- | Adjacent via convoy iff there is a nontrivial path over water provinces
--   from first province to second.
convoyAdjacent :: Province -> Province -> Bool
convoyAdjacent = convoyAdjacent' []

-- Must use the 'used' name to track provinces already visited, so that we don't
-- get caught in loops in our adjacency function.
convoyAdjacent' used prv0 prv1 =
  let waterNeighbours = filter (\x -> isWater x && (not (elem x used))) (adjacency prv0)
  in any (\x -> (x `adjacent` prv1) || (convoyAdjacent' (x : used) x prv1)) waterNeighbours

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

-- ORDER VALIDITY --

-- | All of the reasons why an order can be invalid.
--   TBD maybe we want to give more detailed reasons?
data InvalidReason
  = SubjectDoesNotOccupy Country Unit ProvinceTarget
  | ObjectDoesNotOccupy Country Unit ProvinceTarget
  | CannotMove Unit ProvinceTarget ProvinceTarget
  | CannotSupport Unit ProvinceTarget (Maybe ProvinceTarget)
  | ArmyCannotConvoy ProvinceTarget
    deriving (Show)

type OrderValidity = Maybe InvalidReason

invalidIfFalse :: InvalidReason -> Bool -> OrderValidity
invalidIfFalse _ True = valid 
invalidIfFalse r False = invalid r

invalidIfTrue :: InvalidReason -> Bool -> OrderValidity
invalidIfTrue r b = invalidIfFalse r (not b)

isValid :: OrderValidity -> Bool
isValid Nothing = True
isValid _ = False

isInvalid :: OrderValidity -> Bool
isInvalid = not . isValid

valid :: OrderValidity
valid = Nothing

invalid :: InvalidReason -> OrderValidity
invalid = Just

orderValid :: Board -> Country -> Order -> OrderValidity
orderValid board country order =
  let subject = orderSubject order
      object = orderObject order
  in    (orderSubjectValid board country subject) 
    <|> (orderObjectValid board country subject object)

orderSubjectValid :: Board -> Country -> OrderSubject -> OrderValidity
orderSubjectValid board country (OrderSubject unit pt) =
  invalidIfFalse (SubjectDoesNotOccupy country unit pt) (occupies board (align unit country) pt)

-- We assume the OrderSubject is valid in this one.
-- We cannot verify an OrderObject without an OrderSubject!
-- TODO must use the country to eliminate self-destructive supports.
-- First, though, we must find the relevant rule in the specification.
orderObjectValid :: Board -> Country -> OrderSubject -> OrderObject -> OrderValidity
orderObjectValid board country _ Hold = valid
orderObjectValid board country (OrderSubject unit pt0) (Move pt1) =
  invalidIfFalse (CannotMove unit pt0 pt1) (unitCanMoveFromTo unit pt0 pt1)
orderObjectValid board country (OrderSubject unit0 pt0) (Support unit1 pt1 (Just pt2)) =
  -- Can't support through a convoy, hence the last clause: supporter must be
  -- able to move to the target which it supports, without convoy.
      invalidIfFalse (ObjectDoesNotOccupy country unit1 pt1) (occupies board (align unit1 country) pt1)
  <|> invalidIfFalse (CannotMove unit1 pt1 pt2) (unitCanMoveFromTo unit1 pt1 pt2)
  <|> invalidIfFalse (CannotSupport unit0 pt0 (Just pt2)) (unitCanMoveFromToNoConvoy unit0 pt0 pt2)
  -- Eliminate support against one's own unit.
  -- TBD where in the rule book is this stated?
  -- I am not confident that this is an invalid order... I think it needs to
  -- pass through to resolution.
  -- <|> invalidIfTrue (CannotSupport unit0 pt1 (Just pt2)) (countryOccupies board country pt2)
orderObjectValid board country (OrderSubject unit0 pt0) (Support unit1 pt1 Nothing) =
      invalidIfFalse (ObjectDoesNotOccupy country unit1 pt1) (occupies board (align unit1 country) pt1)
  <|> invalidIfFalse (CannotSupport unit0 pt1 Nothing) (unitCanMoveFromToNoConvoy unit0 pt0 pt1)
--
orderObjectValid board country (OrderSubject unit0 pt0) (Convoy unit1 pt1 pt2) =
      invalidIfFalse (ArmyCannotConvoy pt0) (isFleet unit0)
  <|> invalidIfFalse (ObjectDoesNotOccupy country unit1 pt1) (occupies board (align unit1 country) pt1)
  <|> invalidIfFalse (CannotMove unit1 pt1 pt2) (unitCanMoveFromToWithConvoy unit1 pt1 pt2)
