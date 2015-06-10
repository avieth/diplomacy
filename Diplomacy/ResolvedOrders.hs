{-|
Module      : 
Description : 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Diplomacy.ResolvedOrders (

    ResolvedOrders

  ) where

import Diplomacy.Aligned
import Diplomacy.Unit
import Diplomacy.Phase
import Diplomacy.OrderObject
import Diplomacy.OrderResolution
import Diplomacy.EachProvinceTarget

type ResolvedOrders (phase :: Phase) =
    EachProvinceTarget (Aligned Unit, SomeResolved OrderObject phase)
