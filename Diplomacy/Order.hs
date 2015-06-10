{-|
Module      : Diplomacy.Order
Description : Definition of an order
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Diplomacy.Order (

    Order(..)

  , SomeOrder(..)

  , orderGreatPower
  , orderSubject
  , orderObject

  , isHold
  , movingFrom
  , movingTo
  , supportsMove

  ) where

import Data.Coerce (coerce)
import Diplomacy.GreatPower
import Diplomacy.Aligned
import Diplomacy.Phase
import Diplomacy.OrderSubject
import Diplomacy.OrderType
import Diplomacy.OrderObject
import Diplomacy.Province

newtype Order (phase :: Phase) (order :: OrderType) = Order {
    outOrder :: Aligned (OrderSubject, OrderObject phase order)
  } deriving (Eq, Show)

coerce' :: Order phase order -> Aligned (OrderSubject, OrderObject phase order)
coerce' = coerce

orderGreatPower :: Order phase order -> GreatPower
orderGreatPower = alignedGreatPower . coerce'

orderSubject :: Order phase order -> OrderSubject
orderSubject = fst . alignedThing . coerce'

orderObject :: Order phase order -> OrderObject phase order
orderObject = snd . alignedThing . coerce'

data SomeOrder phase where
    SomeOrder :: Order phase order -> SomeOrder phase

isHold :: Order Typical Move -> Bool
isHold order = from == to
  where
    to = moveTarget . orderObject $ order
    from = orderSubjectProvinceTarget . orderSubject $ order

movingFrom :: Order Typical Move -> ProvinceTarget
movingFrom = orderSubjectProvinceTarget . orderSubject

movingTo :: Order Typical Move -> ProvinceTarget
movingTo = moveTarget . orderObject

supportsMove :: Order Typical Support -> Order Typical Move -> Bool
supportsMove supportOrder moveOrder = from == from' && to == to'
  where
    from = orderSubjectProvinceTarget (orderSubject moveOrder)
    to = moveTarget (orderObject moveOrder)
    from' = orderSubjectProvinceTarget (supportedOrderSubject (orderObject supportOrder))
    to' = supportTarget (orderObject supportOrder)
