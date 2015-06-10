{-|
Module      : Diplomacy.Unit
Description : Definition of units (armies and fleets)
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Diplomacy.Unit (

    Unit(..)

  ) where

data Unit where
    Army :: Unit
    Fleet :: Unit

deriving instance Eq Unit
deriving instance Ord Unit
deriving instance Show Unit
