{-|
Module      : Diplomacy.EachProvinceTarget
Description : 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Diplomacy.EachProvinceTarget (

    EachProvinceTarget

  ) where

import qualified Data.Map as M
import Diplomacy.Province

type EachProvinceTarget = M.Map ProvinceTarget
