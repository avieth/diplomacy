{-|
Module      : Diplomacy.OrderObject
Description : Definition of OrderObject, which describes what a Subject is to do.
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
  , orderObjectEqual

  , SomeOrderObject(..)

  , moveTarget
  , supportedSubject
  , supportTarget
  , withdrawTarget

  ) where

import Diplomacy.Phase
import Diplomacy.Subject
import Diplomacy.OrderType
import Diplomacy.Province

-- | The objective of an order. Together with an Subject and a GreatPower,
--   this makes a complete order.
data OrderObject (phase :: Phase) (order :: OrderType) where

    MoveObject :: ProvinceTarget -> OrderObject Typical Move
    SupportObject
        :: Subject
        -> ProvinceTarget
        -> OrderObject Typical Support
    ConvoyObject
        -- TODO later, would be cool if we could use type system extensions
        -- to eliminate bogus convoys like convoys of fleets or convoys from/to
        -- water provinces.
        :: Subject
        -> ProvinceTarget
        -> OrderObject Typical Convoy

    WithdrawObject :: ProvinceTarget -> OrderObject Retreat Withdraw
    SurrenderObject :: OrderObject Retreat Surrender

    DisbandObject :: OrderObject Adjust Disband
    BuildObject :: OrderObject Adjust Build
    ContinueObject :: OrderObject Adjust Continue
    -- This is convenient because with it, every unit always has an
    -- order in every phase.

deriving instance Eq (OrderObject phase order)
deriving instance Show (OrderObject phase order)

orderObjectEqual :: OrderObject phase order -> OrderObject phase' order' -> Bool
orderObjectEqual object1 object2 = case (object1, object2) of
    (MoveObject pt1, MoveObject pt2) -> pt1 == pt2
    (SupportObject subj1 pt1, SupportObject subj2 pt2) -> (subj1, pt1) == (subj2, pt2)
    (ConvoyObject subj1 pt1, ConvoyObject subj2 pt2) -> (subj1, pt1) == (subj2, pt2)
    (WithdrawObject pt1, WithdrawObject pt2) -> pt1 == pt2
    (SurrenderObject, SurrenderObject) -> True
    (DisbandObject, DisbandObject) -> True
    (BuildObject, BuildObject) -> True
    (ContinueObject, ContinueObject) -> True
    _ -> False

moveTarget :: OrderObject Typical Move -> ProvinceTarget
moveTarget (MoveObject x) = x

supportedSubject :: OrderObject Typical Support -> Subject
supportedSubject (SupportObject x _) = x

supportTarget :: OrderObject Typical Support -> ProvinceTarget
supportTarget (SupportObject _ x) = x

withdrawTarget :: OrderObject Retreat Withdraw -> ProvinceTarget
withdrawTarget (WithdrawObject x) = x

data SomeOrderObject phase where
    SomeOrderObject :: OrderObject phase order -> SomeOrderObject phase

deriving instance Show (SomeOrderObject phase)
