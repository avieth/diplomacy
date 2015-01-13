{-# LANGUAGE GADTs #-}

module Diplomacy.Orders (

    Orders

  , holds
  , moves
  , supports
  , convoys
  , surrenders
  , withdraws
  , disbands
  , builds

  ) where

import Diplomacy.Order
import Diplomacy.Phase

data Orders phaseType where

  OrdersTypical
    :: [Order Typical Hold]
    -> [Order Typical Move]
    -> [Order Typical Support]
    -> [Order Typical Convoy]
    -> Orders Typical

  OrdersRetreat
    :: [Order Retreat Surrender]
    -> [Order Retreat Withdraw]
    -> Orders Retreat

  OrdersAdjust
    :: [Order Adjust Disband]
    -> [Order Adjust Build]
    -> Orders Adjust

holds :: Orders Typical -> [Order Typical Hold]
holds (OrdersTypical hs _ _ _) = hs

moves :: Orders Typical -> [Order Typical Move]
moves (OrdersTypical _ ms _ _) = ms

supports :: Orders Typical -> [Order Typical Support]
supports (OrdersTypical _ _ ss _) = ss

convoys :: Orders Typical -> [Order Typical Convoy]
convoys (OrdersTypical _ _ _ cs) = cs

surrenders :: Orders Retreat -> [Order Retreat Surrender]
surrenders (OrdersRetreat ss _) = ss

withdraws :: Orders Retreat -> [Order Retreat Withdraw]
withdraws (OrdersRetreat _ ws) = ws

disbands :: Orders Adjust -> [Order Adjust Disband]
disbands (OrdersAdjust ds _) = ds

builds :: Orders Adjust -> [Order Adjust Build]
builds (OrdersAdjust _ bs) = bs
