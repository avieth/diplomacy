{-|
Module      : Diplomacy.OrderObject
Description : 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Diplomacy.OrderObject (

    OrderObject(..)

  , SomeOrderObject(..)

  , moveTarget
  , supportedOrderSubject
  , supportTarget
  , withdrawTarget

  ) where

import Diplomacy.Phase
import Diplomacy.OrderSubject
import Diplomacy.OrderType
import Diplomacy.Province

-- | The objective of an order. Together with an OrderSubject and a GreatPower,
--   this makes a complete order.
data OrderObject (phase :: Phase) (order :: OrderType) where

    MoveObject :: ProvinceTarget -> OrderObject Typical Move
    SupportObject
      :: OrderSubject
      -> ProvinceTarget
      -> OrderObject Typical Support

    WithdrawObject :: ProvinceTarget -> OrderObject Retreat Withdraw
    SurrenderObject :: OrderObject Retreat Surrender

    DisbandObject :: OrderObject Adjust Disband
    BuildObject :: OrderObject Adjust Build

deriving instance Eq (OrderObject phase order)
deriving instance Show (OrderObject phase order)

moveTarget :: OrderObject Typical Move -> ProvinceTarget
moveTarget (MoveObject x) = x

supportedOrderSubject :: OrderObject Typical Support -> OrderSubject
supportedOrderSubject (SupportObject x _) = x

supportTarget :: OrderObject Typical Support -> ProvinceTarget
supportTarget (SupportObject _ x) = x

withdrawTarget :: OrderObject Retreat Withdraw -> ProvinceTarget
withdrawTarget (WithdrawObject x) = x

data SomeOrderObject phase where
    SomeOrderObject :: OrderObject phase order -> SomeOrderObject phase
