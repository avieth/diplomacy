{-|
Module      : Diplomacy.OrderSubject
Description : Definition of OrderSubject
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Diplomacy.OrderSubject (

    OrderSubject
  , orderSubjectUnit
  , orderSubjectProvinceTarget

  ) where

import Diplomacy.Unit
import Diplomacy.Province

-- | Description of the subject of an order. Some examples:
--
--     a. F Bre - Eng
--     b. A Par S A Bre - Pic
--
--   have subjects
--
--     a. (Fleet, Normal Brest)
--     b. (Army, Normal Paris)
--
type OrderSubject = (Unit, ProvinceTarget)

orderSubjectUnit :: OrderSubject -> Unit
orderSubjectUnit (x, _) = x

orderSubjectProvinceTarget :: OrderSubject -> ProvinceTarget
orderSubjectProvinceTarget (_, x) = x
