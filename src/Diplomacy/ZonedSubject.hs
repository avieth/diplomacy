{-|
Module      : Diplomacy.ZonedSubject
Description : Subject with different Eq, Ord instances.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE StandaloneDeriving #-}

module Diplomacy.ZonedSubject (

    ZonedSubjectDull(..)
  , ZonedSubjectSharp(..)

  , zonedSubjectDull
  , zonedSubjectSharp

  ) where

import Diplomacy.Subject
import Diplomacy.Zone

newtype ZonedSubjectDull = ZonedSubjectDull Subject

deriving instance Show ZonedSubjectDull

instance Eq ZonedSubjectDull where
    ZonedSubjectDull (_, pt1) == ZonedSubjectDull (_, pt2) =
        Zone pt1 == Zone pt2

instance Ord ZonedSubjectDull where
    ZonedSubjectDull (_, pt1) `compare` ZonedSubjectDull (_, pt2) =
        Zone pt1 `compare` Zone pt2

zonedSubjectDull :: ZonedSubjectDull -> Subject
zonedSubjectDull (ZonedSubjectDull x) = x

newtype ZonedSubjectSharp = ZonedSubjectSharp Subject

deriving instance Show ZonedSubjectSharp

instance Eq ZonedSubjectSharp where
    ZonedSubjectSharp (u1, pt1) == ZonedSubjectSharp (u2, pt2) =
        Zone pt1 == Zone pt2 && u1 == u2

instance Ord ZonedSubjectSharp where
    ZonedSubjectSharp (u1, pt1) `compare` ZonedSubjectSharp (u2, pt2) =
        case Zone pt1 `compare` Zone pt2 of
            EQ -> u1 `compare` u2
            x -> x

zonedSubjectSharp :: ZonedSubjectSharp -> Subject
zonedSubjectSharp (ZonedSubjectSharp x) = x
