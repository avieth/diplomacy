{-|
Module      : Diplomacy.Dislodgement
Description : Unit dislodgement.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Diplomacy.Dislodgement (

      Dislodgement

    , dislodgementAndOccupation

    ) where

import qualified Data.Map as M
import Diplomacy.Aligned
import Diplomacy.Unit
import Diplomacy.Zone
import Diplomacy.OrderObject
import Diplomacy.Phase
import Diplomacy.Occupation
import Diplomacy.OrderResolution

type Dislodgement = M.Map Zone (Aligned Unit)

-- | Use resolved Typical phase orders to compute the 'Dislodgement' and
--   'Occupation' for the next (Retreat) phase.
dislodgementAndOccupation
    :: M.Map Zone (Aligned Unit, SomeResolved OrderObject Typical)
    -> (Dislodgement, Occupation)
dislodgementAndOccupation zonedResolvedOrders = (dislodgement, occupation)
  where

    currentOccupation :: Occupation
    currentOccupation = M.map (\(a, _) -> a) zonedResolvedOrders

    -- First, compute the occupation delta by checking for successful moves.
    moveOccupation :: Occupation
    stationaryOccupation :: Occupation
    (moveOccupation, stationaryOccupation) = M.foldrWithKey nextOccupationFold (M.empty, M.empty) currentOccupation
    nextOccupationFold
        :: Zone
        -> Aligned Unit
        -> (Occupation, Occupation)
        -> (Occupation, Occupation)
    nextOccupationFold zone aunit (move, stationary) = case M.lookup zone zonedResolvedOrders of
        Just (_, SomeResolved (MoveObject pt, Nothing)) ->
            (M.insert (Zone pt) aunit move, stationary)
        _ ->
            (move, M.insert zone aunit stationary)

    -- The dislodgement is the left-biased intersection of the current
    -- occupation with the change in occupation induced by successful
    -- moves (moveOccupation), as these occupations have been upset by
    -- the moves.
    dislodgement :: Dislodgement
    dislodgement = stationaryOccupation `M.intersection` moveOccupation

    -- The next occupation is the left-biased union of the deltas with
    -- the current occupation
    occupation :: Occupation
    occupation = moveOccupation `M.union` (stationaryOccupation `M.difference` dislodgement)
