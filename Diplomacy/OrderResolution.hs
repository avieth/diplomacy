{-|
Module      : Diplomacy.OrderResolution
Description : 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Diplomacy.OrderResolution (

    Resolved
  , SomeResolved(..)
  , withSomeResolved
  , someResolvedValidOrder

  , FailureReason(..)

  , resolveTypical
  , resolveRetreat
  , resolveAdjust

  ) where

import Data.Monoid
import Data.Maybe
import Data.Many
import Data.MayFail
import Data.Functor.Identity
import Data.Traversable (sequenceA)
import Control.Monad
import Control.Applicative
import Data.List (groupBy, (\\))
import Diplomacy.GreatPower
import Diplomacy.Aligned
import Diplomacy.Unit
import Diplomacy.Phase
import Diplomacy.OrderSubject
import Diplomacy.OrderType
import Diplomacy.OrderObject
import Diplomacy.Order
import Diplomacy.Province
import Diplomacy.Valid
import Diplomacy.MoveOrderGraph

import Debug.Trace

type Resolved (k :: Phase -> OrderType -> *) (phase :: Phase) (order :: OrderType) =
    (k phase order, Maybe (FailureReason phase order))

succeeds :: Order phase order -> Resolved Order phase order
succeeds order = (order, Nothing)

data SomeResolved (k :: Phase -> OrderType -> *) phase where
    SomeResolved :: Resolved k phase order -> SomeResolved k phase

withSomeResolved
  :: (forall order . Resolved k phase order -> t) -> SomeResolved k phase -> t
withSomeResolved f term = case term of
    SomeResolved x -> f x

someResolvedValidOrder :: SomeResolved Order phase -> Valid (SomeOrder phase)
someResolvedValidOrder res = case res of
    SomeResolved (order, _) -> Valid (SomeOrder order)

-- | Enumeration of reasons why an order could not succeed.
data FailureReason (phase :: Phase) (order :: OrderType) where

    -- | The move does not have enough support, i.e. some other move(s) into the
    --   same province have at least as much support as this one.
    MoveInsufficientSupport
      :: [Order Typical Move] -- Moves of equal or greater support.
      -> FailureReason Typical Move

    MoveOverpowered
      :: Order Typical Move -- The move which overpowered this move.
      -> FailureReason Typical Move

    -- | The move is part of an unconvoyed 2-cycle of moves.
    Move2Cycle
      :: Order Typical Move -- The move which completes the 2-cycle.
      -> FailureReason Typical Move

    -- | The move would dislodge the player's own unit.
    MoveSelfDislodge
      :: (Order Typical Move, FailureReason Typical Move)
      -> FailureReason Typical Move

    -- | The supported unit did not give an order consistent with the support
    --   order.
    SupportedOrderNotGiven -- There's nothing to support.
      :: FailureReason Typical Support

    -- | The supporting unit was attacked from a province other than the one
    --   into which the support was directed.
    SupportCut
      :: [Order Typical Move] -- The move(s) which cut it.
      -> FailureReason Typical Support

    -- | The supporting unit was overpowered by a move from the province into
    --   which the support was directed.
    SupportDislodged
      :: Order Typical Move -- The move which dislodged it.
      -> FailureReason Typical Support

    -- | The support strengthens a move against a unit belonging to the same
    --   great power as the issuer.
    SupportAgainstSelf
      :: FailureReason Typical Support

    -- | The unit withdraws into the same province as some other unit(s).
    WithdrawCollision
      :: [Order Retreat Withdraw] -- ^ The conflicting withdraws.
      -> FailureReason Retreat Withdraw

    -- Surrender orders and adjust phase orders can never fail; if they're
    -- valid, they succeed!

deriving instance Show (FailureReason phase order)
deriving instance Eq (FailureReason phase order)

-- | Whereas order validation proceeds one order at a time, order resolution
--   must act on all orders of a given phase.
type OrderResolution phase t =
    [Valid (SomeOrder phase)] -> MayFail (ResolutionError phase) Identity t

-- | Enumeration of reasons why order resolution could not proceed.
data ResolutionError (phase :: Phase) where
    MalformedOrderGraph :: ResolutionError Typical
    MultipleOrdersAtProvince :: [[Valid (SomeOrder phase)]] -> ResolutionError phase
    MultipleDislodgers
      :: [Order Typical Move]
      -> SomeOrder Typical
      -> ResolutionError Typical
    --   Multiple moves to the same province target are judged successful.
    MultipleWinningMoves
      :: [Order Typical Move]
      -> ResolutionError Typical
    --   Multiple moves from the same province target are judged failed.
    MultipleFailedMoves
      :: [Order Typical Move]
      -> ResolutionError Typical

-- | Every list in duplicateOrders vs consists of orders from vs which have the
--   same province as their subjects.
duplicateOrders :: [Valid (SomeOrder phase)] -> [[Valid (SomeOrder phase)]]
duplicateOrders = filter ((> 1) <$> length) . groupBy (\x y -> getProvince x == getProvince y)
  where
    getProvince :: Valid (SomeOrder phase) -> Province
    getProvince validOrder = case outValid validOrder of
        SomeOrder order -> ptProvince . orderSubjectProvinceTarget . orderSubject $ order
resolveMultipleOrdersAtProvince
  :: OrderResolution phase [Valid (SomeOrder phase)]
resolveMultipleOrdersAtProvince validOrders = case duplicateOrders validOrders of
    [] -> passes (Identity validOrders)
    xs -> fails (MultipleOrdersAtProvince xs)

validMoves :: [Valid (SomeOrder Typical)] -> [Valid (Order Typical Move)]
validMoves validOrders = case validOrders of
    [] -> []
    Valid (SomeOrder order) : rest -> case orderObject order of
        MoveObject _ -> Valid order : validMoves rest
        _ -> validMoves rest

validSupports :: [Valid (SomeOrder Typical)] -> [Valid (Order Typical Support)]
validSupports validOrders = case validOrders of
    [] -> []
    Valid (SomeOrder order) : rest -> case orderObject order of
        SupportObject _ _ -> Valid order : validSupports rest
        _ -> validSupports rest

validWithdraws :: [Valid (SomeOrder Retreat)] -> [Valid (Order Retreat Withdraw)]
validWithdraws validOrders = case validOrders of
    [] -> []
    Valid (SomeOrder order) : rest -> case orderObject order of
        WithdrawObject _ -> Valid order : validWithdraws rest
        _ -> validWithdraws rest

conflictingWithdraws
  :: Valid (Order Retreat Withdraw)
  -> [Valid (Order Retreat Withdraw)]
  -> [Valid (Order Retreat Withdraw)]
conflictingWithdraws validWithdraw =
    filter (\x -> x /= validWithdraw && validWithdrawTarget x == validWithdrawTarget validWithdraw)
  where
    validWithdrawTarget = withdrawTarget . orderObject . outValid 

isWinningMoveTo :: Province -> Resolved Order Typical Move -> Bool
isWinningMoveTo p m = case m of
    (order, Nothing) -> ptProvince (movingTo order) == p
    _ -> False

winningMovesTo :: Province -> [Resolved Order Typical Move] -> [Resolved Order Typical Move]
winningMovesTo p = filter (isWinningMoveTo p)

movesTo :: Province -> [Order Typical Move] -> [Order Typical Move]
movesTo p = filter (\x -> (ptProvince (movingTo x) == p))

-- | Resolve the retreat phase.
resolveRetreat :: OrderResolution Retreat [SomeResolved Order Retreat]
resolveRetreat =
        resolveMultipleOrdersAtProvince
    >=> resolveRetreatOrders

resolveRetreatOrders :: OrderResolution Retreat [SomeResolved Order Retreat]
resolveRetreatOrders validOrders =
      sequenceA
    . fmap (resolveRetreatOrder (validWithdraws validOrders))
    $ validOrders
  where
    resolveRetreatOrder
      :: [Valid (Order Retreat Withdraw)]
      -> Valid (SomeOrder Retreat)
      -> MayFail (ResolutionError Retreat) Identity (SomeResolved Order Retreat)
    resolveRetreatOrder withdraws validOrder = case outValid validOrder of
        SomeOrder order -> case orderObject order of
            SurrenderObject -> passes (Identity (SomeResolved (order, Nothing)))
            WithdrawObject _ -> case conflictingWithdraws (Valid order) withdraws of
                [] -> passes (Identity (SomeResolved (order, Nothing)))
                xs -> passes (Identity (SomeResolved (order, Just (WithdrawCollision (fmap outValid xs)))))

-- | Resolve the adjust phase.
resolveAdjust :: OrderResolution Adjust [SomeResolved Order Adjust]
resolveAdjust =
        resolveMultipleOrdersAtProvince
    >=> resolveAdjustOrders

-- | Every Adjust-phase order succeeds.
resolveAdjustOrders :: OrderResolution Adjust [SomeResolved Order Adjust]
resolveAdjustOrders = sequenceA . fmap resolveAdjustOrder
  where
    resolveAdjustOrder
      :: Valid (SomeOrder Adjust)
      -> MayFail (ResolutionError Adjust) Identity (SomeResolved Order Adjust)
    resolveAdjustOrder validOrder = case outValid validOrder of
        SomeOrder order -> passes (Identity (SomeResolved (order, Nothing)))

type MoveOrderGraphWithSupports = (MoveOrderGraph, [Order Typical Support])

-- | All supports which support one of these moves (could be a hold).
relevantSupports :: [Order Typical Move] -> [Order Typical Support] -> [Order Typical Support]
relevantSupports orders = filter (\x -> any (supportsMove x) orders)

-- | In which the tip of a path and all competing moves and their supports
--   are resolved.
analyzeTip
  :: [Order Typical Move]
  -> (LNode Province, Many (LEdge (Order Typical Move)))
  -> TypicalResolution
analyzeTip allMoves (_, edges) = TypicalResolution $ \(unresolvedSupports, resolvedSupports, resolvedMoves) -> do

    -- Assumptions:
    -- (1) All moveOrders have common target.
    -- (2) There is no unresolved move from this target (there may be a
    --     resolved move from this target).
    -- 
    -- Implications:
    -- (4) Supports in supports fail if and only if either of
    --     a. There is a move into its province in otherMoves (cut)
    --     b. There is a successful move into its province in resolvedMoves
    --        (dislodged)
    --
    -- The plan:
    -- 1. Resolve all of supports, using otherMoves and resolvedMoves.
    -- 2. Use these resolution to calclate total support of each of
    --    the moves from (1).
    -- 3. Use those supports to find the dominator (if any).
    -- 4. With the dominator (if any) in hand, we can decide the
    --    resolutions.
    -- We start by resolving all relevant supports. These can be cut only
    -- by other moves 
    let moveOrders = (\(_, _, x) -> x) <$> manyToList edges
    let supporting = relevantSupports moveOrders unresolvedSupports
    localResolvedSupports <- sequenceA $
        resolveLocalSupport allMoves resolvedMoves <$> supporting
    localResolvedMoves <- resolveLocalMoves moveOrders localResolvedSupports resolvedMoves
    --trace (" + " ++ show edges) (return ())
    --trace (" @ " ++ show resolvedMoves) (return ())
    --trace (" - " ++ show localResolvedMoves) (return ())
    return (localResolvedSupports ++ resolvedSupports, localResolvedMoves ++ resolvedMoves)

  where

    resolveLocalSupport
      :: [Order Typical Move]
      -> [Resolved Order Typical Move]
      -> Order Typical Support
      -> MayFail (ResolutionError Typical) Identity (Resolved Order Typical Support)
    resolveLocalSupport moves resolvedMoves order = hoist (\x -> Identity (order, x)) resolution
      where

        supportingFrom = orderSubjectProvinceTarget (orderSubject order)
        supportingPower = orderGreatPower order

        resolution :: MayFail (ResolutionError Typical) Maybe (FailureReason Typical Support)
        resolution = cut <|> dislodged

        cut :: MayFail (ResolutionError Typical) Maybe (FailureReason Typical Support)
        cut = case filter ((/=) supportingPower . orderGreatPower) (movesTo (ptProvince supportingFrom) moves) of
            [] -> passes Nothing
            moves -> passes (Just (SupportCut moves))

        dislodged :: MayFail (ResolutionError Typical) Maybe (FailureReason Typical Support)
        dislodged = case winningMovesTo (ptProvince supportingFrom) resolvedMoves of
            [] -> passes Nothing
            [move] -> passes (Just (SupportDislodged (fst move)))
            moves -> fails (MultipleDislodgers (fmap fst moves) (SomeOrder order))

    resolveLocalMoves
      :: [Order Typical Move]
      -> [Resolved Order Typical Support]
      -> [Resolved Order Typical Move]
      -> MayFail (ResolutionError Typical) Identity [Resolved Order Typical Move]
    resolveLocalMoves moves resolvedSupports resolvedMoves =
        let supportedMoves = calculateSupport resolvedSupports <$> moves
            dominator = dominatingMove supportedMoves
        in  case dominator of
                Nothing -> passes (Identity (bounced moves <$> moves))
                Just (n, x) ->
                    -- dominator still might not move, if the support is 1
                    -- and there's a failed move from the target.
                    if n > 1
                    then passes (Identity ((x, Nothing) : others))
                    else case filter (isFailedMoveFrom (ptProvince (movingTo x))) resolvedMoves of
                             [] -> passes (Identity ((x, Nothing) : others))
                             [failed] -> passes (Identity ((x, Just (MoveInsufficientSupport [fst failed])) : others))
                             many -> fails (MultipleFailedMoves (fst <$> many))
                  where
                    others = overpowered x <$> (moves \\ [x])
                             
      where

        bounced :: [Order Typical Move] -> Order Typical Move -> Resolved Order Typical Move
        bounced moves move = (move, Just (MoveInsufficientSupport (moves \\ [move])))

        overpowered :: Order Typical Move -> Order Typical Move -> Resolved Order Typical Move
        overpowered dominator move = (move, Just (MoveOverpowered dominator))

        isFailedMoveFrom :: Province -> Resolved Order Typical Move -> Bool
        isFailedMoveFrom province (order, resolution) = case resolution of
            Just _ -> ptProvince (movingFrom order) == province
            _ -> False

        calculateSupport
          :: [Resolved Order Typical Support]
          -> Order Typical Move
          -> (Int, Order Typical Move)
        calculateSupport supports move = (length (supporters supports move), move)
          where
            countsAsSupportFor
              :: Resolved Order Typical Support
              -> Order Typical Move
              -> Bool
            countsAsSupportFor (support, resolution) move = case resolution of
                Nothing -> supportsMove support move
                _ -> False
            supporters
              :: [Resolved Order Typical Support]
              -> Order Typical Move
              -> [Order Typical Support]
            supporters resolvedSupports order =
                fst <$> filter ((flip countsAsSupportFor) order) resolvedSupports

        dominatingMove :: [(Int, Order Typical Move)] -> Maybe (Int, Order Typical Move)
        dominatingMove xs = case xs of
            [] -> Nothing
            [x] -> Just x
            xs' -> case foldr dominates (0, Nothing) xs' of
                       (n, Nothing) -> Nothing
                       (n, Just x) -> Just (n, x)
          where
            dominates (i, m) (j, l) =
                if i > j
                then (i, Just m)
                else if j > i
                then (j, l)
                else (i, Nothing)

-- | In which a simple cycle in a graph with no paths is resolved.
analyzeCycle :: Many (LEdge (Order Typical Move)) -> TypicalResolution
analyzeCycle cycle = TypicalResolution $ \(unresolvedSupports, resolvedSupports, resolvedMoves) -> case cycle of
    -- In this case it's a 2-cycle; those always fail.
    Many (_, _, x) (One (_, _, y)) -> passes (Identity (resolvedSupports, failX : failY : resolvedMoves))
      where
        failX :: Resolved Order Typical Move
        failX = (x, Just (Move2Cycle y))
        failY :: Resolved Order Typical Move
        failY = (y, Just (Move2Cycle x))
    -- Any other cycle (holds included) succeeds.
    ncycle -> passes (Identity (resolvedSupports, (succeeds . edgeMove <$> manyToList ncycle) ++ resolvedMoves))

fail2Cycle :: TypicalResolution
fail2Cycle = TypicalResolution $ \(_, resolvedSupports, resolvedMoves) -> do
    resolvedMoves' <- sequenceA (failIf2Cycle resolvedMoves <$> resolvedMoves)
    return (resolvedSupports, resolvedMoves')
  where
    failIf2Cycle
      :: [Resolved Order Typical Move]
      -> Resolved Order Typical Move
      -> MayFail (ResolutionError Typical) Identity (Resolved Order Typical Move)
    failIf2Cycle resolvedMoves (order, resolution) = case filter (is2Cycle order . fst) resolvedMoves of
        [] -> passes (Identity (order, resolution))
        [(order', _)] -> passes (Identity (order, Just (Move2Cycle order')))
        -- TODO more specific error.
        _ -> fails MalformedOrderGraph

is2Cycle :: Order Typical Move -> Order Typical Move -> Bool
is2Cycle order1 order2 =
       movingFrom order1 == movingTo order2
    && movingTo order1 == movingFrom order2

failSelfDislodge :: TypicalResolution
failSelfDislodge = TypicalResolution $ \(s, resolvedSupports, resolvedMoves) -> 
    -- A graph of all of the successful moves.
    let graph = makeMoveOrderGraph (fst <$> filter (isNothing . snd) resolvedMoves)
    in  case analyzeMoveOrderGraph failSelfDislodge' (const mempty) graph of
            Left () -> fails MalformedOrderGraph
            Right resolution -> runTypicalResolution resolution (s, resolvedSupports, resolvedMoves)

failSelfDislodge'
  :: (LNode Province, Many (LEdge (Order Typical Move)))
  -> TypicalResolution
failSelfDislodge' (_, edges) = TypicalResolution $ \(_, resolvedSupports, resolvedMoves) -> 
    case edgeMove <$> edges of
        One order -> case mapMaybe (isSelfDislodge order) otherMoves of
            [] -> passes (Identity (resolvedSupports, resolvedMoves))
            [(order', resolution')] -> passes (Identity (resolvedSupports, (order, Just (MoveSelfDislodge (order', resolution'))) : otherMoves))
          where
            otherMoves = resolvedMoves \\ [(order, Nothing)]
        moves -> fails (MultipleWinningMoves (manyToList moves))
  where

isSelfDislodge
  :: Order Typical Move
  -> Resolved Order Typical Move
  -> Maybe (Order Typical Move, FailureReason Typical Move)
isSelfDislodge order (order', resolution) = case resolution of
    Just reason ->
        if    movingTo order == movingFrom order'
           && orderGreatPower order == orderGreatPower order'
        then Just (order', reason)
        else Nothing
    _ -> Nothing

failRemainingSupports :: TypicalResolution
failRemainingSupports = TypicalResolution $ \(supports, resolvedSupports, resolvedMoves) -> do
    let failedSupports = (\x -> (x, Just SupportedOrderNotGiven)) <$> supports
    return (failedSupports ++ resolvedSupports, resolvedMoves)

newtype TypicalResolution = TypicalResolution {
    runTypicalResolution
      -- Input is 
      --   yet unresolved supports
      --   resolved supports so far
      --   resolved orders so far
      -- Output (in MayFail) is
      --   resolved supports at this step plus resolved supports
      --   resolved moves at this step plus resolved moves
      -- Yes, it's unfortunate that you must append the resolved moves at
      -- this step to the input resolved moves.
      :: ([Order Typical Support], [Resolved Order Typical Support], [Resolved Order Typical Move])
      -> MayFail
           (ResolutionError Typical)
           Identity
           ([Resolved Order Typical Support], [Resolved Order Typical Move])
  }

instance Monoid TypicalResolution where
    mempty = TypicalResolution $ \(s, rs, rm) -> passes (Identity (rs, rm))
    x `mappend` y = TypicalResolution $ \(s, rs, rm) -> do
        (rs', rm') <- runTypicalResolution x (s, rs, rm)
        let s' = removeResolvedSupports rs' s
        (rs'', rm'') <- runTypicalResolution y (s', rs', rm')
        return $ (rs'', rm'')
      where
        removeResolvedSupports
          :: [Resolved Order Typical Support]
          -> [Order Typical Support]
          -> [Order Typical Support]
        removeResolvedSupports resolved remaining = remaining \\ (fst <$> resolved)

-- | Resolution of the typical phase orders proceeds in 4 stages:
--
--   - First phase: take tip of path, calculate dominator, set him to
--     successful and others to bounced; if no dominator, set any hold to
--     successful and others to failed. Continue with cycles in the usual way,
--     passing n-cycles and failing 2-cycles.
--   - Second phase: revise all successful moves. If they are actually part
--     of 2-cycles, set to fail.
--   - Third phase: revise all successful moves again. If they would dislodge
--     a friendly own unit, set to fail.
--     Ah but this third phase is sensitive to order! We could use a graph
--     analysis again. Yeah, how about that, build a graph of all successful
--     moves.
--   - Third phase: resolve all remaining supports (they fail).

makeTypicalResolution :: MoveOrderGraph -> Either () TypicalResolution
makeTypicalResolution g = do
    stage1 <- analyzeMoveOrderGraph (analyzeTip (allMoves g)) analyzeCycle g
    stage2 <- return fail2Cycle
    stage3 <- return failSelfDislodge
    stage4 <- return failRemainingSupports
    return $ stage1 <> stage2 <> stage3 <> stage4

evalTypicalResolution
  :: [Order Typical Support]
  -> TypicalResolution
  -> MayFail
       (ResolutionError Typical)
       Identity
       ([Resolved Order Typical Support], [Resolved Order Typical Move])
evalTypicalResolution supports = (flip runTypicalResolution) (supports, [], [])

resolveTypicalOrders
  :: OrderResolution Typical [SomeResolved Order Typical]
resolveTypicalOrders validOrders =
    let moves = outValid <$> validMoves validOrders
        supports = outValid <$> validSupports validOrders
        moveGraph = makeMoveOrderGraph moves
        resolution = makeTypicalResolution moveGraph
    in  case resolution of
            -- TODO have the graph give a witness to the problem so we
            -- can pass it through here.
            Left () -> fails MalformedOrderGraph
            Right resolution' ->
                let resolved = evalTypicalResolution supports resolution'
                in  concatenate <$> resolved
        
  where
    concatenate (resolvedSupports, resolvedMoves) =
        (SomeResolved <$> resolvedSupports) ++ (SomeResolved <$> resolvedMoves)

-- | Resolve the typical phase.
resolveTypical :: OrderResolution Typical [SomeResolved Order Typical]
resolveTypical =
        resolveMultipleOrdersAtProvince
    >=> resolveTypicalOrders
