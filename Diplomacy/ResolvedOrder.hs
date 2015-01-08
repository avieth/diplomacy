{-# LANGUAGE GADTs #-}

module Diplomacy.ResolvedOrder (

    ResolvedOrder(..)
  , OrderFailed(..)
  , OrderSucceeded(..)

  , orderSucceeded
  , orderFailed
  , resolvedOrder

  ) where

import Control.Applicative

import Diplomacy.Phase
import Diplomacy.Province
import Diplomacy.Unit
import Diplomacy.Country
import Diplomacy.Order
import Diplomacy.Orders

-- | An order is either invalid, fails, or succeeds.
--   This is described by a ResolvedOrder.
data ResolvedOrder phaseType
  = Failed (OrderFailed phaseType)
    -- ^ A Failed order is a valid order which could not be completed due to
    --   external factors (other orders).
  | Succeeded (OrderSucceeded phaseType)

orderSucceeded :: ResolvedOrder phaseType -> Bool
orderSucceeded (Succeeded _) = True
orderSucceeded _ = False

orderFailed :: ResolvedOrder phaseType -> Bool
orderFailed (Failed _) = True
orderFailed _ = False

-- | Uniformly treat a ResolvedOrder, no matter what case.
resolvedOrder
  :: (OrderFailed phaseType -> a)
  -> (OrderSucceeded phaseType -> a)
  -> ResolvedOrder phaseType
  -> a
resolvedOrder ifFailed _ (Failed ordf) = ifFailed ordf
resolvedOrder _ ifSucceeded (Succeeded ords) = ifSucceeded ords

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
