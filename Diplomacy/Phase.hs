{-|
Module      : Diplomacy.Phase
Description : Definition of phases of play
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}

module Diplomacy.Phase (

    Phase(..)

  ) where

data Phase where
    Typical :: Phase
    Retreat :: Phase
    Adjust :: Phase
