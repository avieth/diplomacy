{-|
Module      : Diplomacy.Control
Description : Definition of control of provinces.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

module Diplomacy.Control (

    Control

  , emptyControl
  , control
  , controller

  ) where

import qualified Data.Map as M
import Diplomacy.Province
import Diplomacy.GreatPower

-- | Indicates which GreatPower most recently had a unit on a given Province
--   at the beginning of an adjust phase.
type Control = M.Map Province GreatPower

emptyControl :: Control
emptyControl = M.empty

control :: Province -> Maybe GreatPower -> Control -> Control
control pr mgp = M.alter (const mgp) pr

controller :: Province -> Control -> Maybe GreatPower
controller = M.lookup
