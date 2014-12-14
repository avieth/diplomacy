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

  , makeOrder

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
  deriving (Show)

-- | Instruction to move to some ProvinceTarget
data Move = Move ProvinceTarget
  deriving (Show)

-- | Instruction to support some unit from some ProvinceTarget possibly into
--   another ProvinceTarget (Nothing means support a hold).
data Support = Support Unit ProvinceTarget (Maybe ProvinceTarget)
  deriving (Show)

-- | Instruction to convoy some unit from some ProvinceTarget to some
--   ProvinceTarget
data Convoy = Convoy Unit ProvinceTarget ProvinceTarget
  deriving (Show)

-- | Instruction to surrender (disband after dislodgement).
data Surrender = Surrender
  deriving (Show)

-- | Instruction to retreat after dislodgement to some ProvinceTarget.
--   We call it Withdraw because Retreat is already a phase type.
data Withdraw = Withdraw ProvinceTarget
  deriving (Show)

-- | Instruction to disband during adjustment phase.
data Disband = Disband
  deriving (Show)

-- | Instruction to build during adjustment phase (the thing being built is
--   determined by some OrderSubject.
data Build = Build
  deriving (Show)

-- | Subject of an order; the thing which is ordered.
--   We describe it just like it's described in the rules:
--     F London
--     A Berlin
--     etc.
data OrderSubject = OrderSubject Unit ProvinceTarget
  deriving (Show)

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

instance Show (Order phaseType) where
  show (HoldOrder c os h) = "HoldOrder (" ++ (show c) ++ " " ++ (show os) ++ " " ++ (show h) ++ ")"
  show (MoveOrder c os m) = "MoveOrder (" ++ (show c) ++ " " ++ (show os) ++ " " ++ (show m) ++ ")"
  show (SupportOrder c os s) = "SupportOrder (" ++ (show c) ++ " " ++ (show os) ++ " " ++ (show s) ++ ")"
  show (ConvoyOrder c os co) = "ConvoyOrder (" ++ (show c) ++ " " ++ (show os) ++ " " ++ (show co) ++ ")"
  show (SurrenderOrder c os s) = "SurrenderOrder (" ++ (show c) ++ " " ++ (show os) ++ " " ++ (show s) ++ ")"
  show (WithdrawOrder c os w) = "WithdrawOrder (" ++ (show c) ++ " " ++ (show os) ++ " " ++ (show w) ++ ")"
  show (DisbandOrder c os d) = "DisbandOrder (" ++ (show c) ++ " " ++ (show os) ++ " " ++ (show d) ++ ")"
  show (BuildOrder c os b) = "BuildOrder (" ++ (show c) ++ " " ++ (show os) ++ " " ++ (show b) ++ ")"

makeOrder :: Country -> OrderSubject -> OrderObject phaseType -> Order phaseType
makeOrder country subject (HoldObject h) = HoldOrder country subject h
makeOrder country subject (MoveObject m) = MoveOrder country subject m
makeOrder country subject (SupportObject s) = SupportOrder country subject s
makeOrder country subject (ConvoyObject c) = ConvoyOrder country subject c
makeOrder country subject (SurrenderObject s) = SurrenderOrder country subject s
makeOrder country subject (WithdrawObject w) = WithdrawOrder country subject w
makeOrder country subject (DisbandObject d) = DisbandOrder country subject d
makeOrder country subject (BuildObject b) = BuildOrder country subject b

data OrderObject phaseType where

  HoldObject :: Hold -> OrderObject Typical
  MoveObject :: Move -> OrderObject Typical
  SupportObject :: Support -> OrderObject Typical
  ConvoyObject :: Convoy -> OrderObject Typical

  SurrenderObject :: Surrender -> OrderObject Retreat
  WithdrawObject :: Withdraw -> OrderObject Retreat

  DisbandObject :: Disband -> OrderObject Adjust
  BuildObject :: Build -> OrderObject Adjust

instance Show (OrderObject phaseType) where
  show (HoldObject h) = "HoldObject " ++ (show h) ++ ")"
  show (MoveObject m) = "MoveObject (" ++ (show m) ++ ")"
  show (SupportObject s) = "SupportObject " ++ (show s) ++ ")"
  show (ConvoyObject c) = "ConvoyObject " ++ (show c) ++ ")"
  show (SurrenderObject s) = "SurrenderObject " ++ (show s) ++ ")"
  show (WithdrawObject w) = "WithdrawObject " ++ (show w) ++ ")"
  show (DisbandObject d) = "DisbandObject " ++ (show d) ++ ")"
  show (BuildObject b) = "BuildObject " ++ (show b) ++ ")"

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

