{-# LANGUAGE GADTs #-}

module Diplomacy.OrderInvalid (

    OrderInvalid(..)

  ) where

import Diplomacy.Phase
import Diplomacy.Province
import Diplomacy.Unit
import Diplomacy.Order
import Diplomacy.Country

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
