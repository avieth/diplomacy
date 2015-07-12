{-|
Module      : Diplomacy.Board
Description : 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Diplomacy.Board (

  ) where

type Board = M.Map Zone (Aligned Unit)

type BoardWithOrders phase = M.Map Zone (Aligned Unit, SomeOrder phase)
