module Diplomacy.Resolve (

    resolveOrderTypical
  , resolveOrderRetreat
  , resolveOrderAdjust

  ) where

import Diplomacy.Phase
import Diplomacy.Board
import Diplomacy.Order
import Diplomacy.Orders
import Diplomacy.ResolvedOrder
import Diplomacy.ResolvedOrders

-- | This type indicates that in order to resolve a typical phase order, all we
--   need is a set of orders (other orders given in that same phase), a board
--   against which to resolve this order, and the order itself.
resolveOrderTypical
  :: Orders Typical
  -> Board Typical
  -> Order Typical
  -> ResolvedOrder Typical
resolveOrderTypical = undefined

-- | This type indicates that in order to resolve a retreat phase order, all we
--   need is a set of orders (other orders given in that same phase), a board
--   against which to resolve this order, the resolutions of the previous
--   phase, and the order itself.
resolveOrderRetreat
  :: Orders Retreat
  -> Board Retreat
  -> ResolvedOrders Typical
  -> Order Retreat
  -> ResolvedOrder Retreat
resolveOrderRetreat = undefined

-- | This type indicates that in order to resolve an adjust phase order, all we
--   need is the board against which it is to be resolved, and the order itself.
resolveOrderAdjust
  :: Board Adjust
  -> Order Adjust
  -> ResolvedOrder Adjust
resolveOrderAdjust = undefined
