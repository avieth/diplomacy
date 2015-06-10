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

  , occupies
  , occupy

  ) where

import qualified Data.Map as M
import Diplomacy.Aligned
import Diplomacy.Unit
import Diplomacy.Province
import Diplomacy.EachProvinceTarget

type Occupation = EachProvinceTarget (Aligned Unit)

occupies :: ProvinceTarget -> Aligned Unit -> Occupation -> Bool
occupies pt aunit occupation = maybe False (== aunit) (M.lookup pt occupation)

occupy :: ProvinceTarget -> Aligned Unit -> Occupation -> Occupation
occupy = M.insert
