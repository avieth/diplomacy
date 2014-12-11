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

  , orderSubjectUnit
  , orderSubjectTarget

  , Order

  , order
  , orderSubject
  , orderObject

  , AlignedOrder

  , alignOrder
  , alignedOrderCountry
  , alignedOrder

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

-- | An order, parameterized by the phase type to which it is relevant
--   The principal use of this datatype is to give a phase type to each of
--   the order datatypes described above.
data OrderObject phaseType where

  HoldObject :: Hold -> OrderObject Typical
  MoveObject :: Move -> OrderObject Typical
  SupportObject :: Support -> OrderObject Typical
  ConvoyObject :: Convoy -> OrderObject Typical

  SurrenderObject :: Surrender -> OrderObject Retreat
  -- ^ We call it surrender because disband is taken by the adjustment phase
  --   order.
  WithdrawObject :: Withdraw -> OrderObject Retreat

  DisbandObject :: Disband -> OrderObject Adjust
  BuildObject :: Build -> OrderObject Adjust

newtype Order a = Order (OrderSubject, OrderObject a)

orderSubject :: Order a -> OrderSubject
orderSubject (Order (os, _)) = os

orderObject :: Order a -> OrderObject a
orderObject (Order (_, oo)) = oo

order :: OrderSubject -> OrderObject a -> Order a
order os oo = Order (os, oo)

-- | An order paired with a country, meaning the country which issued it.
newtype AlignedOrder a = AlignedOrder (Country, Order a)

alignedOrderCountry :: AlignedOrder a -> Country
alignedOrderCountry (AlignedOrder (c, _)) = c

alignedOrder :: AlignedOrder a -> Order a
alignedOrder (AlignedOrder (_, o)) = o

alignOrder :: Country -> Order a -> AlignedOrder a
alignOrder c o = AlignedOrder (c, o)
