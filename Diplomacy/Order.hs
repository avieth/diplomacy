{-# LANGUAGE GADTs #-}

module Diplomacy.Order (

    Order

  , OrderSubject(..)

  , orderSubjectUnit
  , orderSubjectTarget

  , OrderObject(..)

  , order
  , orderSubject
  , orderObject

  ) where

import Diplomacy.Phase
import Diplomacy.Unit
import Diplomacy.Province

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
data OrderObject a where

  Hold :: OrderObject Typical
  Move :: ProvinceTarget -> OrderObject Typical
  Support :: Unit -> ProvinceTarget -> (Maybe ProvinceTarget) -> OrderObject Typical
  -- ^ Second one is maybe because we can support a hold.
  Convoy :: Unit -> ProvinceTarget -> ProvinceTarget -> OrderObject Typical

  Surrender :: OrderObject Retreat
  -- ^ We call it surrender because disband is taken by the adjustment phase
  --   order.
  Retreat :: ProvinceTarget -> OrderObject Retreat

  Disband :: OrderObject Adjust
  Build :: Unit -> OrderObject Adjust

instance Eq (OrderObject a) where
  Hold == Hold = True
  (Move t) == (Move t') = t == t'
  (Support u pt ptt) == (Support u' pt' ptt') = u == u && pt == pt' && ptt == ptt'
  (Convoy u pt ptt) == (Convoy u' pt' ptt') = u == u && pt == pt' && ptt == ptt'
  Surrender == Surrender = True
  (Retreat pt) == (Retreat pt') = pt == pt'
  Disband == Disband = True
  (Build u) == (Build u') = u == u'
  _ == _ = False

-- | Description of an order; just a subject with an object.
newtype Order a = Order (OrderSubject, OrderObject a)
  deriving (Eq)

instance Ord (Order a) where
  o <= s = orderSubject o <= orderSubject s

order :: OrderSubject -> OrderObject a -> Order a
order s o = Order (s, o)

orderSubject :: Order a -> OrderSubject
orderSubject (Order (s, _)) = s

orderObject :: Order a -> OrderObject a
orderObject (Order (_, t)) = t
