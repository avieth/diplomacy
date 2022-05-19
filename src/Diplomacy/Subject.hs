{-|
Module      : Diplomacy.Subject
Description : Definition of Subject
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

module Diplomacy.Subject (

    Subject
  , subjectUnit
  , subjectProvinceTarget

  ) where

import Diplomacy.Unit
import Diplomacy.Province

-- | Description of a subject in a diplomacy game, like the subject of an order
--   for instance:
--
--     a. F Bre - Eng
--     b. A Par S A Bre - Pic
--
--   have subjects
--
--     a. (Fleet, Normal Brest)
--     b. (Army, Normal Paris)
--
type Subject = (Unit, ProvinceTarget)

subjectUnit :: Subject -> Unit
subjectUnit (x, _) = x

subjectProvinceTarget :: Subject -> ProvinceTarget
subjectProvinceTarget (_, x) = x
