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
{-# LANGUAGE StandaloneDeriving #-}

module Diplomacy.Order (

    Order(..)

  , SomeOrder(..)

  , orderSubject
  , orderObject

  , isHold
  , movingFrom
  , movingTo
  , supportsOrder

  ) where

import Data.Coerce (coerce)
import Diplomacy.GreatPower
import Diplomacy.Aligned
import Diplomacy.Phase
import Diplomacy.Subject
import Diplomacy.OrderType
import Diplomacy.OrderObject
import Diplomacy.Province

newtype Order (phase :: Phase) (order :: OrderType) = Order {
    outOrder :: (Subject, OrderObject phase order)
  } deriving (Eq, Show)

coerce' :: Order phase order -> (Subject, OrderObject phase order)
coerce' = coerce

orderSubject :: Order phase order -> Subject
orderSubject = fst . coerce'

orderObject :: Order phase order -> OrderObject phase order
orderObject = snd . coerce'

data SomeOrder phase where
    SomeOrder :: Order phase order -> SomeOrder phase

instance Eq (SomeOrder phase) where
    SomeOrder o1 == SomeOrder o2 = case (orderObject o1, orderObject o2) of
        (MoveObject _, MoveObject _) -> o1 == o2
        (SupportObject _ _, SupportObject _ _) -> o1 == o2
        _ -> False

deriving instance Show (SomeOrder phase)

isHold :: Order Typical Move -> Bool
isHold order = from == to
  where
    to = moveTarget . orderObject $ order
    from = subjectProvinceTarget . orderSubject $ order

movingFrom :: Order Typical Move -> ProvinceTarget
movingFrom = subjectProvinceTarget . orderSubject

movingTo :: Order Typical Move -> ProvinceTarget
movingTo = moveTarget . orderObject

supportsOrder :: OrderObject Typical Support -> SomeOrder Typical -> Bool
supportsOrder supportOrderObject (SomeOrder order) =
       supportedSubject supportOrderObject == orderSubject order
    && supportTarget supportOrderObject == orderDestination order
  where
    orderDestination :: Order Typical order -> ProvinceTarget
    orderDestination order = case orderObject order of
        MoveObject pt -> pt
        SupportObject _ _ -> subjectProvinceTarget (orderSubject order)
