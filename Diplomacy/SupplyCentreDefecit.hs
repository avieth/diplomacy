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

  , supplyCentreDefecit

  ) where

import qualified Data.Map as M
import Diplomacy.GreatPower
import Diplomacy.Occupation
import Diplomacy.Control
import Diplomacy.Province
import Diplomacy.Aligned
import Diplomacy.Unit

type SupplyCentreDefecit = Int

supplyCentreDefecit
    :: GreatPower
    -> Occupation
    -> Control
    -> SupplyCentreDefecit
supplyCentreDefecit greatPower occupation control = unitCount - supplyCentreCount
  where
    unitCount = M.fold unitCountFold 0 occupation
    supplyCentreCount = M.foldWithKey supplyCentreCountFold 0 control
    unitCountFold :: Aligned Unit -> Int -> Int
    unitCountFold aunit
        | alignedGreatPower aunit == greatPower = (+) 1
        | otherwise = id
    supplyCentreCountFold :: Province -> GreatPower -> Int -> Int
    supplyCentreCountFold pr greatPower'
        |    greatPower' == greatPower
          && elem pr supplyCentres = (+) 1
        | otherwise = id
