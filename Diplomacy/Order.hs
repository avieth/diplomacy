module Diplomacy.Order (

    Order

  , OrderSubject(..)
  , OrderObject(..)

  , order
  , orderSubject
  , orderObject

  ) where

import Diplomacy.Unit
import Diplomacy.Province

-- | Subject of an order; the thing which is ordered.
--   We describe it just like it's described in the rules:
--     F London
--     A Berlin
--     etc.
data OrderSubject = OrderSubject Unit ProvinceTarget
  deriving (Eq, Ord, Show)

-- | Object of an order; the action to be carried out by some subject.
--   We describe it just like it's described in the rules:
--     H
--     M Paris (written "-Paris")
--     S F London-NorthSea
--     C A Spain-Naples
data OrderObject
  = Hold
  | Move ProvinceTarget
  | Support Unit ProvinceTarget ProvinceTarget
  | Convoy Unit ProvinceTarget ProvinceTarget
    deriving (Eq, Ord, Show)

-- | Description of an order; just a subject with an object.
newtype Order = Order (OrderSubject, OrderObject)
  deriving (Eq, Ord, Show)

order :: OrderSubject -> OrderObject -> Order
order s o = Order (s, o)

orderSubject :: Order -> OrderSubject
orderSubject (Order (s, _)) = s

orderObject :: Order -> OrderObject
orderObject (Order (_, t)) = t
