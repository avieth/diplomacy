{-|
Module      : Diplomacy.Season
Description : 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Diplomacy.Season (

    Season(..)

  ) where

data Season = Spring | Fall | Winter
  deriving (Eq, Show)
