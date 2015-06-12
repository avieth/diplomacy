{-|
Module      : Diplomacy.MoveOrderGraph
Description : Definition of a graph of move orders over provinces.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Diplomacy.MoveOrderGraph (

    MoveOrderGraph
  , makeMoveOrderGraph
  , analyzeMoveOrderGraph
  , allMoves
  , edgeMove

  , module G

  ) where

import Prelude hiding (foldr)
import Control.Applicative as A
import Data.Foldable
import Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Analysis.Algorithms.Common
import Data.Proxy
import Data.Many
import Data.Maybe
import Data.Monoid
import Diplomacy.Order
import Diplomacy.Phase
import Diplomacy.Aligned
import Diplomacy.GreatPower
import Diplomacy.Unit
import Diplomacy.Order
import Diplomacy.OrderObject
import Diplomacy.OrderType
import Diplomacy.Province

-- | Construct a graph from an enumerable, bounded thing, with a node for each
--   of its members.
graphFromEnum
  :: forall t edgeLabel .
     ( Enum t
     , Bounded t
     )
  => Gr t edgeLabel
graphFromEnum = foldr insNode G.empty enumeration
  where
    enumeration = fmap (makeLNode . fromEnum) ([minBound .. maxBound] :: [t])
    makeLNode x = (x, toEnum x)

-- | A graph with each Province as a node.
provinceGraph :: Gr Province edgeLabel
provinceGraph = graphFromEnum

type MoveOrderGraph = Gr Province (Order Typical Move)

makeMoveOrderGraph :: [Order Typical Move] -> MoveOrderGraph
makeMoveOrderGraph = foldr insertMove provinceGraph

-- | Insert a move order into a graph on Province nodes.
insertMove
  :: Order Typical Move
  -> Gr Province (Order Typical Move)
  -> Gr Province (Order Typical Move)
insertMove order = insEdge edge
  where
    edge = (fromEnum from, fromEnum to, order)
    from = ptProvince (movingFrom order)
    to = ptProvince (movingTo order)

-- | Get all nodes which are the tip of some path which is *not* contained in
--   some simple cycle.
--   But how to do this?
--
--   g ----> a ---> b ---> c <--- d <--- f
--                  ^-----/
--
--   We want, in this case, to pick out either b or c.
--   We DO know that every node has out-degree AT MOST 1.
--   But, sometimes we still want a node with out-degree 1; to figure out
--     whether we do, we must determine whether there's a cycle there.
--     Easy, follow the other one and if it loops, use it.
--   
getTips :: Gr a b -> [(LNode a, Many (LEdge b))]
getTips g = mapMaybe (getOneTip g) (labNodes g)
  where
    getOneTip :: Gr a b -> LNode a -> Maybe (LNode a, Many (LEdge b))
    getOneTip g node@(n, _) = case (out g n, inn g n) of
        -- 0 out-degree means we're done.
        ([], e : es) -> Just (node, makeMany e es)
        -- 1 out-degree means we may have what we need. If in-degree is
        -- 1 then we don't want it.
        ([_], [_]) -> Nothing
        -- But if in-degree is > 1 then we check whether this node touches a
        -- cycle.
        ([o], e : es) -> case getCycleAtEdge o g of
                             Just _ -> Just (node, makeMany e es)
                             Nothing -> Nothing
        _ -> Nothing

-- | Get one node with 0 out-degree, >0 in-degree, along with its incoming
--   edges, or Nothing if there is no such node.
getTip :: Gr a b -> Maybe (LNode a, Many (LEdge b))
getTip g = case getTips g of
    x : _ -> Just x
    _ -> Nothing

-- | Get the cycles *in a graph which consists only of simple cycles*. If you
--   use this on a graph which does *not* consist *only* of *simple* cycles,
--   then you get Nothing.
getCycles :: Eq b => Gr a b -> Maybe [Many (LEdge b)]
getCycles g = case labEdges g of
    e : _ -> do
        cycle <- getCycleAtEdge e g
        rest <- getCycles (removeCycle cycle g)
        return $ cycle : rest
    _ -> Just []

-- | Get the cycle beginning at a given edge in a graph which consists only of
--   simple cycles.
getCycleAtEdge :: LEdge b -> Gr a b -> Maybe (Many (LEdge b))
getCycleAtEdge e = getCycleAtEdge' [e] e e
  where
    getCycleAtEdge'
      :: [LEdge b] -- the edges seen so far
      -> LEdge b -- the initial edge
      -> LEdge b -- the current edge
      -> Gr a b
      -> Maybe (Many (LEdge b))
    getCycleAtEdge' soFar s@(initial, _, _) (_, terminus, label) g =
        case out g terminus of
            [e@(_, terminus', _)] ->
                if terminus' == initial
                then Just (makeMany e soFar)
                else getCycleAtEdge' (e : soFar) s e g
            -- In this case there is an edge with >1 out-degree! That's not
            -- an edge in a graph consisting only of simple cycles!
            _ -> Nothing

-- | Get one cycle *in a graph which consists only of simple cycles*.
--   Nothing in case there are no cycles (in which case the graph must have
--   no edges).
--   You get Left in case the graph is malformed.
getCycle :: Eq b => Gr a b -> Either () (Maybe (Many (LEdge b)))
getCycle g = case getCycles g of
    Just (x : _) -> Right (Just x)
    Just [] -> Right Nothing
    _ -> Left ()

-- | Remove a node and its incoming edges from a graph (should be called with
--   output of @getTip@ or @getTips@).
removeTip :: Eq b => (LNode a, Many (LEdge b)) -> Gr a b -> Gr a b
removeTip (_, es) g = foldr delLEdge g es

-- | Remove a cycle from a graph (should be called with out put of
--   @getCycle@ or @getCycles@.
removeCycle :: Eq b => Many (LEdge b) -> Gr a b -> Gr a b
removeCycle es g = foldr delLEdge g es

-- | Analyze a graph by carrying out some monoidal computation defined for
--   tips and cycles. The computation is left-to-right tip-to-cycle.
--
--   This must only be used on a graph such that every node has out-degree
--   *at most 1*. This ensures that all cycles are simple, and so @getCycles@
--   makes sense. You get Left () in case the graph is malformed.
analyzeMoveOrderGraph
  :: ( Eq b, Monoid t)
  => ((LNode a, Many (LEdge b)) -> t)
  -> (Many (LEdge b) -> t)
  -> Gr a b
  -> Either () t
analyzeMoveOrderGraph noCycle cycle g = case (getTip g, getCycle g) of
    (Nothing, Left ()) -> Left ()
    (Nothing, Right Nothing) -> Right mempty
    (Nothing, Right (Just c)) -> mappend (cycle c) <$> rest
      where
        rest = analyzeMoveOrderGraph noCycle cycle (removeCycle c g)
    (Just ti, _) -> mappend (noCycle ti) <$> rest
      where
        rest = analyzeMoveOrderGraph noCycle cycle (removeTip ti g)

allMoves :: MoveOrderGraph -> [Order Typical Move]
allMoves = fmap (\(_, _, x) -> x) . labEdges

edgeMove :: LEdge (Order Typical Move) -> Order Typical Move
edgeMove (_, _, x) = x

move1 :: Order Typical Move
move1 = Order (align ((Fleet, Normal London), MoveObject (Normal EnglishChannel)) England)

move2 :: Order Typical Move
move2 = Order (align ((Fleet, Normal EnglishChannel), MoveObject (Normal Brest)) England)

move3 :: Order Typical Move
move3 = Order (align ((Army, Normal Brest), MoveObject (Normal Paris)) England)

move4 :: Order Typical Move
move4 = Order (align ((Army, Normal Paris), MoveObject (Normal Gascony)) England)

move5 :: Order Typical Move
move5 = Order (align ((Army, Normal Portugal), MoveObject (Normal Spain)) England)

move6 :: Order Typical Move
move6 = Order (align ((Army, Normal Spain), MoveObject (Normal Marseilles)) England)

move7 :: Order Typical Move
move7 = Order (align ((Army, Normal Marseilles), MoveObject (Normal Gascony)) England)

move8 :: Order Typical Move
move8 = Order (align ((Army, Normal Munich), MoveObject (Normal Berlin)) Germany)

move9 :: Order Typical Move
move9 = Order (align ((Army, Normal Berlin), MoveObject (Normal Bohemia)) Germany)

move10 :: Order Typical Move
move10 = Order (align ((Army, Normal Bohemia), MoveObject (Normal Munich)) Germany)

move11 :: Order Typical Move
move11 = Order (align ((Army, Normal Moscow), MoveObject (Normal StPetersburg)) Germany)

move12 :: Order Typical Move
move12 = Order (align ((Army, Normal StPetersburg), MoveObject (Normal Moscow)) Germany)

graph =
    insertMove move1
  . insertMove move2
  . insertMove move3
  . insertMove move4
  . insertMove move5
  . insertMove move6
  . insertMove move7
  . insertMove move8
  . insertMove move9
  $ graphFromEnum

cycleGraph =
    insertMove move8
  . insertMove move9
  . insertMove move10
  . insertMove move11
  . insertMove move12
  $ graphFromEnum
