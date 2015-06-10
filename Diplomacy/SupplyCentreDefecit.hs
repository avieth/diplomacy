{-|
Module      : Diplomacy.SupplyCentreDefecit
Description : 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Diplomacy.SupplyCentreDefecit (

    SupplyCentreDefecit

  , isNegative

  ) where

type SupplyCentreDefecit = Int

isNegative :: SupplyCentreDefecit -> Bool
isNegative x = x < 0
