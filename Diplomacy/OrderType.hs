{-|
Module      : Diplomacy.OrderType
Description : Definition of order types
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}

module Diplomacy.OrderType (

    OrderType(..)

  ) where

-- | Enumeration of types of orders. Useful when DataKinds is enabled.
data OrderType where
    Move :: OrderType
    Support :: OrderType
    Convoy :: OrderType
    Withdraw :: OrderType
    Surrender :: OrderType
    Disband :: OrderType
    Build :: OrderType
    Continue :: OrderType
