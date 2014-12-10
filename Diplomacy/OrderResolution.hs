{-# LANGUAGE GADTs #-}

module Diplomacy.OrderResolution (

    OrderResolution
  , OrderInvalid(..)
  , OrderFailed(..)
  , OrderSucceeded(..)

  ) where

import Diplomacy.Phase
import Diplomacy.Province
import Diplomacy.Unit

-- | An order is either invalid, fails, or succeeds.
data OrderResolution a = OrderInvalid a | OrderFailed a | OrderSucceeded a

-- | Any order can be invalid. Each witness gives a reason.
data OrderInvalid a where

  HoldInvalid :: OrderInvalidReason HoldInvalidReason -> OrderInvalid Typical
  MoveInvalid :: OrderInvalidReason MoveInvalidReason -> OrderInvalid Typical
  SupportInvalid :: OrderInvalidReason SupportInvalidReason -> OrderInvalid Typical
  ConvoyInvalid :: OrderInvalidReason ConvoyInvalidReason -> OrderInvalid Typical

  SurrenderInvalid :: OrderInvalidReason SurrenderInvalidReason -> OrderInvalid Retreat
  RetreatInvalid :: OrderInvalidReason RetreatInvalidReason -> OrderInvalid Retreat

  DisbandInvalid :: OrderInvalidReason DisbandInvalidReason -> OrderInvalid Adjust
  BuildInvalid :: OrderInvalidReason BuildInvalidReason -> OrderInvalid Adjust

-- | Some orders can fail. Each witness gives a reason.
data OrderFailed a where

  HoldFailed :: HoldDislodgedReason -> OrderFailed Typical
  MoveFailed :: MoveBouncedReason -> OrderFailed Typical
  SupportFailed :: SupportCutReason -> OrderFailed Typical
  ConvoyFailed :: ConvoyFailedReason -> OrderFailed Typical

  RetreatFailed :: RetreatFailedReason -> OrderFailed Retreat

  BuildFailed :: BuildFailedReason -> OrderFailed Adjust

-- | Any order can succeed. Each witness gives no reason.
data OrderSucceeded a where

  HoldSucceeded :: OrderSucceeded Typical
  MoveSucceeded :: OrderSucceeded Typical
  SupportSucceeded :: OrderSucceeded Typical
  ConvoySucceeded :: OrderSucceeded Typical

  SurrenderSucceeded :: OrderSucceeded Retreat
  RetreatSucceeded :: OrderSucceeded Retreat

  DisbandSucceeded :: OrderSucceeded Adjust
  BuildSucceeded :: OrderSucceeded Adjust

data OrderInvalidReason a
  = SubjectInvalid OrderSubjectInvalidReason
  | OtherReason a

data OrderSubjectInvalidReason = UnitNotPresent ProvinceTarget Unit

data MoveInvalidReason
  = MoveTargetUnreachable

-- | There is no special reason why a hold could be invalid.
--   If the subject of this order is valid, then the order is valid.
data HoldInvalidReason

data SupportInvalidReason
  = SupportAgainstSelf

data ConvoyInvalidReason
  = NotConvoyAdjacent

data SurrenderInvalidReason
  = UnitNotDislodged ProvinceTarget Unit

data RetreatInvalidReason
  = RetreatTargetNotAdjacent
  | RetreatIntoAttackingProvince

-- | There is no special reason why a disband could be invalid.
--   If the subject of this order is valid, then the order is valid.
data DisbandInvalidReason

-- | There is no special reason why a build could be invalid.
--   If the subject of this order is valid, then the order is valid.
data BuildInvalidReason

data HoldDislodgedReason
  = Overpowered ProvinceTarget AlignedUnit

data MoveBouncedReason
  = Standoff ProvinceTarget AlignedUnit

data SupportCutReason
  = Attacked ProvinceTarget AlignedUnit

data ConvoyFailedReason
  = ConvoyingFleetDislodged Province ProvinceTarget AlignedUnit

data RetreatFailedReason
  = RetreatConflict ProvinceTarget AlignedUnit

data BuildFailedReason
  = InlandFleet ProvinceTarget
  | InsufficientSupplyCentres ProvinceTarget
