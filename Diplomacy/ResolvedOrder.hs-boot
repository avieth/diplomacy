{-# LANGUAGE RoleAnnotations #-}

module Diplomacy.ResolvedOrder (

    ResolvedOrder
  , OrderFailed
  , OrderSucceeded
  , ValidOrder
  , resolveOrders

  ) where

import Diplomacy.Order

type role ResolvedOrder nominal
data ResolvedOrder phaseType

type role OrderFailed nominal
data OrderFailed phaseType

type role OrderSucceeded nominal
data OrderSucceeded phaseType

type role ValidOrder nominal
newtype ValidOrder phaseType = ValidOrder {
    validOrder :: Order phaseType
  }

resolveOrders :: [ValidOrder phaseType] -> [ResolvedOrder phaseType]
