{-# LANGUAGE GADTs #-}

module Diplomacy.Order (

    Hold(..)
  , Move(..)
  , Support(..)
  , Convoy(..)
  , Surrender(..)
  , Withdraw(..)
  , Disband(..)
  , Build(..)

  , OrderSubject(..)
  , OrderObject(..)

  , defaultOrderObjectTypical
  , defaultOrderObjectRetreat

  , orderSubjectUnit
  , orderSubjectTarget

  , Order(..)

  , orderCountry
  , orderSubject
  , orderObject

  ) where

import Diplomacy.Phase
import Diplomacy.Unit
import Diplomacy.Province
import Diplomacy.Country

-- | Instruction to hold.
data Hold = Hold

-- | Instruction to move to some ProvinceTarget
data Move = Move ProvinceTarget

-- | Instruction to support some unit from some ProvinceTarget possibly into
--   another ProvinceTarget (Nothing means support a hold).
data Support = Support Unit ProvinceTarget (Maybe ProvinceTarget)

-- | Instruction to convoy some unit from some ProvinceTarget to some
--   ProvinceTarget
data Convoy = Convoy Unit ProvinceTarget ProvinceTarget

-- | Instruction to surrender (disband after dislodgement).
data Surrender = Surrender

-- | Instruction to retreat after dislodgement to some ProvinceTarget.
--   We call it Withdraw because Retreat is already a phase type.
data Withdraw = Withdraw ProvinceTarget

-- | Instruction to disband during adjustment phase.
data Disband = Disband

-- | Instruction to build during adjustment phase (the thing being built is
--   determined by some OrderSubject.
data Build = Build

-- | Subject of an order; the thing which is ordered.
--   We describe it just like it's described in the rules:
--     F London
--     A Berlin
--     etc.
data OrderSubject = OrderSubject Unit ProvinceTarget
  deriving (Eq, Ord, Show)

orderSubjectUnit :: OrderSubject -> Unit
orderSubjectUnit (OrderSubject u _) = u

orderSubjectTarget :: OrderSubject -> ProvinceTarget
orderSubjectTarget (OrderSubject _ pt) = pt

-- | An order, parameterized by the phase type to which it is relevant.
--   It completely characterizes an order: country issuing the order, subject
--   of the order, and object of the order (hold, move, build, etc.)
data Order phaseType where

  HoldOrder :: Country -> OrderSubject -> Hold -> Order Typical
  MoveOrder :: Country -> OrderSubject -> Move -> Order Typical
  SupportOrder :: Country -> OrderSubject -> Support -> Order Typical
  ConvoyOrder :: Country -> OrderSubject -> Convoy -> Order Typical

  SurrenderOrder :: Country -> OrderSubject -> Surrender -> Order Retreat
  WithdrawOrder :: Country -> OrderSubject -> Withdraw -> Order Retreat

  DisbandOrder :: Country -> OrderSubject -> Disband -> Order Adjust
  BuildOrder :: Country -> OrderSubject -> Build -> Order Adjust

data OrderObject phaseType where

  HoldObject :: Hold -> OrderObject Typical
  MoveObject :: Move -> OrderObject Typical
  SupportObject :: Support -> OrderObject Typical
  ConvoyObject :: Convoy -> OrderObject Typical

  SurrenderObject :: Surrender -> OrderObject Retreat
  WithdrawObject :: Withdraw -> OrderObject Retreat

  DisbandObject :: Disband -> OrderObject Adjust
  BuildObject :: Build -> OrderObject Adjust

defaultOrderObjectTypical :: OrderObject Typical
defaultOrderObjectTypical = HoldObject Hold

defaultOrderObjectRetreat :: OrderObject Retreat
defaultOrderObjectRetreat = SurrenderObject Surrender

orderCountry :: Order phaseType -> Country
orderCountry (HoldOrder c _ _) = c
orderCountry (MoveOrder c _ _) = c
orderCountry (SupportOrder c _ _) = c
orderCountry (ConvoyOrder c _ _) = c
orderCountry (SurrenderOrder c _ _) = c
orderCountry (WithdrawOrder c _ _) = c
orderCountry (DisbandOrder c _ _) = c
orderCountry (BuildOrder c _ _) = c

orderSubject :: Order phaseType -> OrderSubject
orderSubject (HoldOrder _ s _) = s
orderSubject (MoveOrder _ s _) = s
orderSubject (SupportOrder _ s _) = s
orderSubject (ConvoyOrder _ s _) = s
orderSubject (SurrenderOrder _ s _) = s
orderSubject (WithdrawOrder _ s _) = s
orderSubject (DisbandOrder _ s _) = s
orderSubject (BuildOrder _ s _) = s

orderObject :: Order phaseType -> OrderObject phaseType
orderObject (HoldOrder _ _ h) = HoldObject h
orderObject (MoveOrder _ _ m) = MoveObject m
orderObject (SupportOrder _ _ s) = SupportObject s
orderObject (ConvoyOrder _ _ c) = ConvoyObject c
orderObject (SurrenderOrder _ _ s) = SurrenderObject s
orderObject (WithdrawOrder _ _ w) = WithdrawObject w
orderObject (DisbandOrder _ _ d) = DisbandObject d
orderObject (BuildOrder _ _ b) = BuildObject b

