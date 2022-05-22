{-|
Module      : Diplomacy.OrderObject
Description : Definition of OrderObject, which describes what a Subject is to do.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

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
  , convoySubject
  , convoyTarget
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

    -- Adjust phase is a bit weird because the BuildObject is for units which
    -- don't actually exist. You can undo a Disband by giving a Continue, but
    -- you can't undo a Build by giving anything.
    -- FIXME we could probably do better, by having build orders held in
    -- a separate structure. For now, a game server implementation can require
    -- that all build orders be given in a batch, clearing any previous ones.
    BuildObject :: OrderObject Adjust Build
    DisbandObject :: OrderObject Adjust Disband
    -- | This is convenient because with it, every unit always has an
    -- order in every phase.
    ContinueObject :: OrderObject Adjust Continue

deriving instance Eq (OrderObject phase order)
deriving instance Show (OrderObject phase order)

instance Ord (OrderObject phase order) where
    x `compare` y = case (x, y) of
        (MoveObject pt, MoveObject pt') -> pt `compare` pt'
        (SupportObject subj pt, SupportObject subj' pt') -> (subj, pt) `compare` (subj, pt')
        (ConvoyObject subj pt, ConvoyObject subj' pt') -> (subj, pt) `compare` (subj', pt')
        (SurrenderObject, SurrenderObject) -> EQ
        (WithdrawObject pt, WithdrawObject pt') -> pt `compare` pt'
        (DisbandObject, DisbandObject) -> EQ
        (BuildObject, BuildObject) -> EQ
        (ContinueObject, ContinueObject) -> EQ

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

convoySubject :: OrderObject Typical Convoy -> Subject
convoySubject (ConvoyObject x _) = x

convoyTarget :: OrderObject Typical Convoy -> ProvinceTarget
convoyTarget (ConvoyObject _ x) = x

withdrawTarget :: OrderObject Retreat Withdraw -> ProvinceTarget
withdrawTarget (WithdrawObject x) = x

data SomeOrderObject phase where
    SomeOrderObject :: OrderObject phase order -> SomeOrderObject phase

deriving instance Show (SomeOrderObject phase)

{-
instance Eq (SomeOrderObject phase) where
    (SomeOrderObject x) == (SomeOrderObject y) = case (x, y) of
        (MoveObject _, MoveObject _) -> x == y
        (SupportObject _ _, SupportObject _ _) -> x == y
        (ConvoyObject _ _, ConvoyObject _ _) -> x == y
        (SurrenderObject, SurrenderObject) -> x == y
        (WithdrawObject _, WithdrawObject _) -> x == y
        (DisbandObject, DisbandObject) -> x == y
        (BuildObject, BuildObject) -> x == y
        (ContinueObject, ContinueObject) -> x == y
-}
