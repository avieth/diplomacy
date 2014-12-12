{-# LANGUAGE GADTs #-}

module Diplomacy.ResolvedOrder (

    ResolvedOrder
  , OrderInvalid(..)
  , OrderFailed(..)
  , OrderSucceeded(..)

  , orderSucceeded
  , orderFailed
  , orderInvalid
  , resolvedOrder

  ) where

import Control.Applicative

import Diplomacy.Phase
import Diplomacy.Province
import Diplomacy.Unit
import Diplomacy.Board
import Diplomacy.Country
import Diplomacy.Order
import Diplomacy.Orders

-- | An order is either invalid, fails, or succeeds.
--   This is described by an ResolvedOrder.
data ResolvedOrder phaseType
  = Invalid (OrderInvalid phaseType)
    -- ^ Invalid means that the order could never go through, even if there
    --   were no other orders out there. Contrast with the explanation of
    --   Failed.
  | Failed (OrderFailed phaseType)
    -- ^ A Failed order is a valid order which could not be completed due to
    --   external factors (other orders).
  | Succeeded (OrderSucceeded phaseType)

orderSucceeded :: ResolvedOrder phaseType -> Bool
orderSucceeded (Succeeded _) = True
orderSucceeded _ = False

orderFailed :: ResolvedOrder phaseType -> Bool
orderFailed (Failed _) = True
orderFailed _ = False

orderInvalid :: ResolvedOrder phaseType -> Bool
orderInvalid (Invalid _) = True
orderInvalid _ = False

-- | Uniformly treat a ResolvedOrder, no matter what case.
resolvedOrder
  :: (OrderInvalid phaseType -> a)
  -> (OrderFailed phaseType -> a)
  -> (OrderSucceeded phaseType -> a)
  -> ResolvedOrder phaseType
  -> a
resolvedOrder ifInvalid _ _ (Invalid ordi) = ifInvalid ordi
resolvedOrder _ ifFailed _ (Failed ordf) = ifFailed ordf
resolvedOrder _ _ ifSucceeded (Succeeded ords) = ifSucceeded ords

-- | Any order can be invalid. Each witness gives a reason.
--   The type parameter indicates the phase to which it is relevant.
data OrderInvalid phaseType where

  SubjectInvalid
    :: Order phaseType 
    -- ^ Could be any order.
    --   For the other constructors, we spell out the components of the order
    --   so that we can guarantee that the order is of the right type.
    -> ProvinceTarget
    -> Unit
    -> OrderInvalid phaseType
    -- ^ Unit not present at the target.

  MoveTargetUnreachable
    :: Country
    -> OrderSubject
    -> Move
    -> ProvinceTarget
    -> ProvinceTarget
    -> OrderInvalid Typical
  UnitCannotOccupy
    :: Country
    -> OrderSubject
    -> Move
    -> Unit
    -> ProvinceTarget
    -> OrderInvalid Typical

  SupportAgainstSelf
    :: Country
    -> OrderSubject
    -> Support
    -> ProvinceTarget
    -> OrderInvalid Typical

  NotConvoyAdjacent
    :: Country
    -> OrderSubject
    -> Convoy
    -> ProvinceTarget
    -> ProvinceTarget
    -> OrderInvalid Typical

  SurrenderUnitNotDislodged
    :: Country
    -> OrderSubject
    -> Surrender
    -> ProvinceTarget
    -> OrderInvalid Retreat

  WithdrawUnitNotDislodged
    :: Country
    -> OrderSubject
    -> Withdraw
    -> ProvinceTarget
    -> OrderInvalid Retreat
  WithdrawIntoAttackingProvince
    :: Country
    -> OrderSubject
    -> Withdraw
    -> ProvinceTarget
    -> ProvinceTarget
    -> OrderInvalid Retreat
  WithdrawTargetNotAdjacent
    :: Country
    -> OrderSubject
    -> Withdraw
    -> ProvinceTarget
    -> ProvinceTarget
    -> OrderInvalid Retreat

  InlandFleet
    :: Country
    -> OrderSubject
    -> Build
    -> ProvinceTarget
    -> OrderInvalid Adjust
  InsufficientSupplyCentres
    :: Country
    -> OrderSubject
    -> Build
    -> ProvinceTarget
    -> OrderInvalid Adjust

-- | Witness of a failed order, indicating that the order would have succeeded
--   if not for some other order.
--   Each value carries the relevant aligned order and more information
--   describing the reason for the failure.
--
--   TBD prehaps include the order(s) which caused this order to fail?
data OrderFailed phaseType where

  HoldOverpowered
    :: Country
    -> OrderSubject
    -> Hold
    -> ProvinceTarget
       -- ^ attacker coming from here
    -> AlignedUnit
       -- ^ attacking with this unit
    -> OrderFailed Typical

  MoveBounced
    :: Country
    -> OrderSubject
    -> Move
    -> OrderFailed Typical

  SupportCut
    :: Country
    -> OrderSubject
    -> Support
    -> ProvinceTarget
       -- ^ Support from
    -> ProvinceTarget
       -- ^ Support unit in this place
    -> ProvinceTarget
       -- ^ Support cut by a move from here
    -> AlignedUnit
       -- ^ By this unit
    -> OrderFailed Typical

  ConvoyFailed
    :: Country
    -> OrderSubject
    -> Convoy
    -> ProvinceTarget
       -- ^ Convoy from here
    -> ProvinceTarget
       -- ^ Convoy to here
    -> ProvinceTarget
       -- ^ Fleet dislodged by attack from here
    -> AlignedUnit
       -- ^ Dislodged by this unit's attack
    -> OrderFailed Typical

  WithdrawConflict
    :: Country
    -> OrderSubject
    -> Withdraw
    -> ProvinceTarget
       -- ^ Tried to withdraw to here
    -> ProvinceTarget
       -- ^ Other guy tried to withdraw from here
    -> AlignedUnit
       -- ^ Other guy
    -> OrderFailed Retreat

-- | Witness that an order succeeded, meaning it is not invalid and did not fail.
data OrderSucceeded phaseType where

  HoldSucceeded
    :: Country
    -> OrderSubject
    -> Hold
    -> OrderSucceeded Typical
  MoveSucceeded
    :: Country
    -> OrderSubject
    -> Move
    -> OrderSucceeded Typical
  SupportSucceeded
    :: Country
    -> OrderSubject
    -> Support
    -> OrderSucceeded Typical
  ConvoySucceeded
    :: Country
    -> OrderSubject
    -> Convoy
    -> OrderSucceeded Typical

  SurrenderSucceeded
    :: Country
    -> OrderSubject
    -> Surrender
    -> OrderSucceeded Retreat
  WithdrawSucceeded
    :: Country
    -> OrderSubject
    -> Withdraw
    -> OrderSucceeded Retreat

  DisbandSucceeded
    :: Country
    -> OrderSubject
    -> Disband
    -> OrderSucceeded Adjust
  BuildSucceeded
    :: Country
    -> OrderSubject
    -> Build
    -> OrderSucceeded Adjust

{-
ok = Nothing
notOk a = Just a

-- TODO tonight. Define this and its dependent functions.
--invalid :: Board a -> Order a -> Maybe (OrderInvalid a)
--invalid b ao = subjectInvalid b ao <|> objectInvalid b ao

-- | For all orders except for build, the order is invalid if the subject is
--   not consistent with the board against which the order is given.
subjectInvalid :: Board a -> Order a -> Maybe (OrderInvalid a)
subjectInvalid b ao = case order ao of
  Build _ -> ok
  ord -> if occupies b au pt then ok else notOk (invalidReason)
    where s = orderSubject ord
          pt = orderSubjectTarget s
          u = orderSubjectUnit s
          c = orderCountry ao
          au = align u c
          invalidReason = case ord of
            Hold x -> HoldInvalid x (SubjectInvalid (UnitNotPresent pt u))
            Move x -> MoveInvalid x (SubjectInvalid (UnitNotPresent pt u))
            Support x -> SupportInvalid x (SubjectInvalid (UnitNotPresent pt u))
            Convoy x -> ConvoyInvalid x (SubjectInvalid (UnitNotPresent pt u))
            Surrender x -> SurrenderInvalid x (SubjectInvalid (UnitNotPresent pt u))
            Withdraw x -> WithdrawInvalid x (SubjectInvalid (UnitNotPresent pt u))
            Disband x -> DisbandInvalid x (SubjectInvalid (UnitNotPresent pt u))

-- | To determine whether the order's object is invalid, we switch based upon
--   the contents of the order.
objectInvalid :: Board a -> AlignedOrder a -> Maybe (OrderInvalid a)
objectInvalid b ao = case order ao of
  Hold holdOrder -> holdObjectInvalid holdOrder
  Move moveOrder -> moveObjectInvalid moveOrder
  Support supportOrder -> supportObjectInvalid supportOrder (orderCountry ao)
  Convoy convoyOrder -> convoyObjectInvalid convoyOrder
  Surrender surrenderOrder -> surrenderObjectInvalid b surrenderOrder
  Retreat retreatOrder -> retreatObjectInvalid retreatOrder
  Disband disbandOrder -> disbandObjectInvalid disbandOrder
  Build buildOrder -> buildObjectInvalid buildOrder

holdObjectInvalid :: Hold -> Maybe (OrderInvalid a)
holdObjectInvalid holdOrder = ok

moveObjectInvalid (Move os pt) = ok

supportObjectInvalid (Support os u pt mpt) c = ok

convoyObjectInvalid (Convoy os u pt pt') = ok

-- | We assume here that the subject is valid, so all we have to do is check
--   that the unit at the subject target province is indeed dislodged; we already
--   know it is aligned to the issuer of the order.
surrenderObjectInvalid
  :: Board Retreat
  -> Surrender
  -> Maybe SurrenderInvalidReason
surrenderObjectInvalid brd surrenderOrder = unitNotDislodged
  where subject = surrenderOrderSubject surrenderOrder
        pt = orderSubjectTarget subject
        unitNotDislodged = case dislodged brd pt of
          Nothing -> notOk (UnitNotDislodged pt)
          _ -> ok

-- Need to know where the attacking force came from in order to resolve this
-- one! How to do that? Give ResolvedOrder Retreat more context somehow...
-- Well, it has access to a Board Retreat, maybe we could put the information
-- there? Seems more just to take the previous Typical phase's resolution
-- instead, so that resolving a retreat phase demands an ResolvedOrder Typical
-- from the previous phase.
retreatObjectInvalid _ = ok

-- | Identifies an invalid build order object.
--   This depends also upon the country issuing the order and the board against
--   which it was issued.
buildObjectInvalid :: Country -> Board Adjust -> Build -> Maybe BuildInvalidReason
buildObjectInvalid c brd buildOrder = inlandFleet <|> insufficientSupplyCentres
  where subject = buildOrderSubject buildOrder
        pt = orderSubjectTarget subject
        pr = ptProvince pt
        un = orderSubjectUnit subject
        inlandFleet =
          if isInland pr && (isFleet un)
          then notOk (InlandFleet pt)
          else ok
        insufficientSupplyCentres =
          if numberOfSupplyCentres c brd <= numberOfUnits c brd
          then notOk InsufficientSupplyCentres
          else ok
-}
