{-|
Module      : Diplomacy.SupplyCentreDeficit
Description : Compute the supply centre deficit for a 'GreatPower'.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

module Diplomacy.SupplyCentreDeficit (

    SupplyCentreDeficit

  , supplyCentreDeficit

  ) where

import qualified Data.Map as M
import Diplomacy.GreatPower
import Diplomacy.Occupation
import Diplomacy.Control
import Diplomacy.Province
import Diplomacy.Aligned
import Diplomacy.Unit

type SupplyCentreDeficit = Int

supplyCentreDeficit
    :: GreatPower
    -> Occupation
    -> Control
    -> SupplyCentreDeficit
supplyCentreDeficit greatPower occupation control = unitCount - supplyCentreCount
  where
    unitCount = M.foldr unitCountFold 0 occupation
    supplyCentreCount = M.foldrWithKey supplyCentreCountFold 0 control
    unitCountFold :: Aligned Unit -> Int -> Int
    unitCountFold aunit
        | alignedGreatPower aunit == greatPower = (+) 1
        | otherwise = id
    supplyCentreCountFold :: Province -> GreatPower -> Int -> Int
    supplyCentreCountFold pr greatPower'
        |    greatPower' == greatPower
          && elem pr supplyCentres = (+) 1
        | otherwise = id
