{-# LANGUAGE GADTs #-}

module Diplomacy.Order (

    Order(..)

  , HoldOrder(..)
  , MoveOrder(..)
  , SupportOrder(..)
  , ConvoyOrder(..)
  , SurrenderOrder(..)
  , RetreatOrder(..)
  , DisbandOrder(..)
  , BuildOrder(..)

  , OrderSubject(..)

  , orderSubjectUnit
  , orderSubjectTarget

  , orderSubject

  , AlignedOrder(..)

  , orderCountry
  , order

  ) where

import Diplomacy.Phase
import Diplomacy.Unit
import Diplomacy.Province
import Diplomacy.Country

data HoldOrder = HoldOrder OrderSubject
data MoveOrder = MoveOrder OrderSubject ProvinceTarget
data SupportOrder = SupportOrder OrderSubject Unit ProvinceTarget (Maybe ProvinceTarget)
data ConvoyOrder = ConvoyOrder OrderSubject Unit ProvinceTarget ProvinceTarget
data SurrenderOrder = SurrenderOrder OrderSubject
data RetreatOrder = RetreatOrder OrderSubject ProvinceTarget
data DisbandOrder = DisbandOrder OrderSubject
data BuildOrder = BuildOrder OrderSubject

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

-- | Object of an order; the action to be carried out by some subject.
--   We describe it just like it's described in the rules:
--     H
--     M Paris (written "-Paris")
--     S F London-NorthSea
--     S A Munich
--     C A Spain-Naples
data Order a where

  Hold :: HoldOrder -> Order Typical
  Move :: MoveOrder -> Order Typical
  Support :: SupportOrder -> Order Typical
  -- ^ Second one is maybe because we can support a hold.
  Convoy :: ConvoyOrder -> Order Typical

  Surrender :: SurrenderOrder -> Order Retreat
  -- ^ We call it surrender because disband is taken by the adjustment phase
  --   order.
  Retreat :: RetreatOrder -> Order Retreat

  Disband :: DisbandOrder -> Order Adjust
  Build :: BuildOrder -> Order Adjust

orderSubject :: Order a -> OrderSubject
orderSubject (Hold (HoldOrder os)) = os
orderSubject (Move (MoveOrder os _)) = os
orderSubject (Support (SupportOrder os _ _ _)) = os
orderSubject (Convoy (ConvoyOrder os _ _ _)) = os
orderSubject (Surrender (SurrenderOrder os)) = os
orderSubject (Retreat (RetreatOrder os _)) = os
orderSubject (Disband (DisbandOrder os)) = os
orderSubject (Build (BuildOrder os)) = os

data AlignedOrder a = AlignedOrder Country (Order a)

orderCountry :: AlignedOrder a -> Country
orderCountry (AlignedOrder c _) = c

order :: AlignedOrder a -> Order a
order (AlignedOrder _ o) = o
