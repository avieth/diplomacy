{-|
Module      : Diplomacy.Occupation
Description : 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Diplomacy.Occupation (

    Occupation

  , emptyOccupation
  , occupy

  , occupies
  , unitOccupies

  ) where

import qualified Data.Map as M
import Diplomacy.Aligned
import Diplomacy.Unit
import Diplomacy.Province
import Diplomacy.EachProvinceTarget

type Occupation = EachProvinceTarget (Aligned Unit)

emptyOccupation :: Occupation
emptyOccupation = M.empty

occupy :: ProvinceTarget -> Aligned Unit -> Occupation -> Occupation
occupy = M.insert

occupies :: ProvinceTarget -> Aligned Unit -> Occupation -> Bool
occupies pt aunit occupation = maybe False (== aunit) (M.lookup pt occupation)

unitOccupies :: ProvinceTarget -> Unit -> Occupation -> Bool
unitOccupies pt unit occupation = maybe False ((==) unit . alignedThing) (M.lookup pt occupation)
