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
  , someResolvedValidOrder

  , ResolvedOrders

  , FailureReason(..)

  , resolveTypical
  , resolveRetreat
  , resolveAdjust

  ) where

--import qualified Data.Map as M
import Data.Monoid
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
import Diplomacy.EachProvinceTarget
import Diplomacy.Valid

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

type ResolvedOrders phase =
    EachProvinceTarget (Aligned Unit, SomeResolved OrderObject phase)

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

-- | Whereas order validation proceeds one order at a time, order resolution
--   must act on all orders of a given phase.
type OrderResolution phase t =
    [Valid (SomeOrder phase)] -> MayFail (ResolutionError phase) Identity t

-- | Enumeration of reasons why order resolution could not proceed.
data ResolutionError (phase :: Phase) where
    MultipleOrdersAtProvince :: [[Valid (SomeOrder phase)]] -> ResolutionError phase
    MultipleDislodgers
      :: [Order Typical Move]
      -> SomeOrder Typical
      -> ResolutionError Typical
    -- | Multiple moves to the same province target are judged successful.
    MultipleWinningMoves
      :: [Order Typical Move]
      -> ResolutionError Typical
    MultipleFailedMovesFromProvince
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


{-
type Orders phase = EachProvinceTarget (Aligned Unit, SomeOrderObject phase)
type ValidOrders phase = EachProvinceTarget (Aligned Unit, Valid (SomeOrderObject phase))
type OrderSet phase = [SomeOrder phase]
type ValidOrderSet phase = [Valid (SomeOrder phase)]
type ResolvedOrderSet phase = [SomeResolved Order phase]

orderSet :: Orders phase -> OrderSet phase
orderSet = M.foldWithKey makeSomeOrder []
  where
    makeSomeOrder target (aunit, someObject) rest = case someObject of
        SomeOrderObject object ->
            let subject = (alignedThing aunit, target)
                power = alignedGreatPower aunit
                order = Order $ align (subject, object) power
            in  SomeOrder order : rest

validOrderSet :: ValidOrders phase -> ValidOrderSet phase
validOrderSet = M.foldWithKey makeSomeValidOrder []
  where
    makeSomeValidOrder target (aunit, someObject) rest = case someObject of
        Valid (SomeOrderObject object) ->
            let subject = (alignedThing aunit, target)
                power = alignedGreatPower aunit
                order = Order $ align (subject, object) power
            in  Valid (SomeOrder order) : rest

resolvedOrderSet :: ResolvedOrders phase -> ResolvedOrderSet phase
resolvedOrderSet = M.foldWithKey makeSomeResolvedOrder []
  where
    makeSomeResolvedOrder target (aunit, someObject) rest = case someObject of
        SomeResolved (object, resolution) ->
            let subject = (alignedThing aunit, target)
                power = alignedGreatPower aunit
                order = Order $ align (subject, object) power
            in  SomeResolved (order, resolution) : rest
-}

-- | The move orders of a typical phase of a diplomacy match form a certain
--   kind of graph. The provinces are nodes, the moves are edges, and
--   each node has out-degree at most 1, since there can be at most one order
--   per province. So, we're left with a graph factored into paths and
--   cycles, such that each edge appears in at most one path or cycle, and
--   these paths and cycles are *maximal*, they are not proper subsets of
--   other paths or cycles in the graph.
--
--   This structure is used to incrementally judge the move orders of a typical
--   phase. It controls the structure of the typical phase resolution
--   computation.
--   - If there are no paths, then every cycle except for unconvoyed
--     2-cycles succeeds.
--   - If there are paths, we can resolve the tip of any of the paths by
--     calculating the support of it and of every other move into its target.
--     This resolves all moves with that target, their supports, as well as
--     support coming from the move target in case of dislodgement.
--
--  Assumption: there are no duplicates. If x is in cycles or paths, then it
--  appears in only one of them and only once.
--
data MoveOrderGraph where
    MoveOrderGraph :: Cycles -> Paths -> MoveOrderGraph

deriving instance Show MoveOrderGraph

type Cycles = [Cycle]
type Paths = [Path]

-- | Path is left-to-right head-to-tail so that
--     x <- y <- z
--   has representation
--     Many x (Many y (One z))
type Path = Many (Order Typical Move)
-- | Like a Path, but we're responsible for proving that every Cycle has
--
--       orderSubjectProvinceTarget . orderSubect . outValid . mlast
--     = moveTarget . orderObject . outValid . mfirst
--
type Cycle = Many (Order Typical Move)

emptyMoveOrderGraph :: MoveOrderGraph
emptyMoveOrderGraph = MoveOrderGraph [] []

-- Check for multiple orders at province before using this.
--
-- NB a hold is a move where target and source coincide; we count it as a
-- cycle here.
addMove :: Order Typical Move -> MoveOrderGraph -> MoveOrderGraph
addMove order (MoveOrderGraph ps cs) =
    let ps' = extendPaths order ps
        (ps'', cs') = eliminateCycles ps'
    in  if isHold order
        then MoveOrderGraph ps (One order : cs)
        else MoveOrderGraph ps'' (cs' ++ cs)

-- | Put a valid move into a list of paths, either extending an existing path
--   or making a new one (possibly a cycle, in case target and source coincide
--   as in a hold). This can be rectified by running eliminateCycles on the
--   output.
extendPaths :: Order Typical Move -> Paths -> Paths
extendPaths order ps = case ps of
    [] -> [One order]
    path : rest ->
        let to = moveTarget . orderObject $ order
            from = orderSubjectProvinceTarget . orderSubject $ order
            to' = moveTarget . orderObject . mfirst $ path
            from' = orderSubjectProvinceTarget . orderSubject . mlast $ path
        in  if to' == from
            then manyCons order path : rest
            else if from' == to
            then manySnoc path order : rest
            else path : extendPaths order rest

eliminateCycles :: Paths -> (Paths, Cycles)
eliminateCycles ps = case ps of
    [] -> ([], [])
    path : rest ->
        if to == from
        -- In this then branch we add path to the cycles list, so we need to
        -- guarantee that it's really a cycle, i.e. that
        --
        --       orderSubjectProvinceTargetTarget . orderSubect . outValid . mlast $ path
        --     = moveTarget . orderObject . outValid . mfirst $ path
        --
        -- but that's exactly what we've done (see the where clause).
        then (ps', path : cs)
        else (path : ps', cs)
      where
        (ps', cs) = eliminateCycles rest
        to = moveTarget . orderObject . mfirst $ path
        from = orderSubjectProvinceTarget . orderSubject . mlast $ path

removeMove :: Order Typical Move -> MoveOrderGraph -> MoveOrderGraph
removeMove order (MoveOrderGraph ps cs) =
    let ps' = removeMoveFromPaths order ps
        (ps'', cs') = removeMoveFromCycles order cs
    in  MoveOrderGraph (ps'' ++ ps') cs'

removeMoveFromPaths :: Order Typical Move -> Paths -> Paths
removeMoveFromPaths order ps = case ps of
    [] -> []
    path : rest ->
        case splitMany order path of
            SplitNotPresent -> path : removeMoveFromPaths order rest
            -- No recursion necessary, since we assume each order appears
            -- at most once.
            SplitEmpty -> rest
            SplitEnd path' -> path' : rest
            SplitMiddle pathLeft pathRight -> pathLeft : pathRight : rest

removeMoveFromCycles :: Order Typical Move -> Cycles -> (Paths, Cycles)
removeMoveFromCycles order cs = case cs of
    [] -> ([], [])
    cycle : rest ->
        case splitMany order cycle of
            SplitNotPresent -> (ps, cycle : cs')
            SplitEmpty -> (ps, cs')
            -- path' must really be a path, since it is cycle with one element
            -- removed (and all cycles that we deal with here are simple
            -- cycles).
            SplitEnd path' -> (path' : ps, cs')
            -- pathLeft and pathRight together form one path.
            -- pathLeft is the left-hand-side of the list, which by convention
            -- is the terminal side of the move, so we want to tack the inital
            -- side onto it (the terminus in pathLeft coincides with the
            -- initiation in pathRight).
            SplitMiddle pathLeft pathRight -> (manyAppend pathRight pathLeft : ps, cs')
      where
        (ps, cs') = removeMoveFromCycles order rest

type MoveOrderGraphWithSupports = (MoveOrderGraph, [Order Typical Support])

-- | Take the tip of some path, and every other move with common target, giving
--   a smaller graph as well. Gives Nothing in case there are no paths in the
--   graph.
takeTipOfPath
  :: MoveOrderGraphWithSupports
  -> Maybe (
         Order Typical Move
       , [Order Typical Move]
       , [Order Typical Support]
       , [Order Typical Move]
       , MoveOrderGraphWithSupports
       )
takeTipOfPath (graph, supports) = case graph of
    MoveOrderGraph [] _ -> Nothing
    MoveOrderGraph (p : _) _ ->
        let allMovesInGraph = moveOrderGraphToList graph
            principalOrder = mfirst p
            competingOrders = competingMoves principalOrder allMovesInGraph
            supporting = relevantSupports (principalOrder : competingOrders) supports
            newGraph = foldr removeMove graph (principalOrder : competingOrders)
            otherMoves = moveOrderGraphToList newGraph
        in  Just (principalOrder, competingOrders, supporting, otherMoves, (newGraph, supports))

-- | All moves which have the same target as some move. Includes holds.
competingMoves :: Order Typical Move -> [Order Typical Move] -> [Order Typical Move]
competingMoves order = filter (/= order) . movesTo (ptProvince (movingTo order))

-- | All supports which support one of these moves (could be a hold).
relevantSupports :: [Order Typical Move] -> [Order Typical Support] -> [Order Typical Support]
relevantSupports orders = filter (\x -> any (supportsMove x) orders)

moveOrderGraphToList :: MoveOrderGraph -> [Order Typical Move]
moveOrderGraphToList graph = case graph of
    MoveOrderGraph ps cs -> (ps >>= manyToList) ++ (cs >>= manyToList)

-- | Take some cycle from a graph
takeCycle :: MoveOrderGraphWithSupports -> Maybe (Cycle, MoveOrderGraphWithSupports)
takeCycle (graph, supports) = case graph of
    MoveOrderGraph _ [] -> Nothing
    MoveOrderGraph ps (c : cs) -> Just (c, (MoveOrderGraph ps cs, supports))

-- | Combine takeTipOfPath and takeCycle such that cycles are taken only if
--   there are no paths.
takeNext
  :: MoveOrderGraphWithSupports
  -> Maybe (
         Either
           (Order Typical Move, [Order Typical Move], [Order Typical Support], [Order Typical Move])
           Cycle
       , MoveOrderGraphWithSupports
       )
takeNext graph = case takeTipOfPath graph of
    Just (v, w, x, y, z) -> Just (Left (v, w, x, y), z)
    Nothing -> case takeCycle graph of
        Just (c, g) -> Just (Right c, g)
        Nothing -> Nothing

-- | Recursion on a MoveOrderGraph. This is guaranteed to terminate, because
--   takeNext always produces a smaller graph.
--
--   In our use case, the monoid t shall be 
--     [Resolved Order Typical Move] -> [Resolved Order Typical Move]
--   and so analyzing a move order graph will give us a function of this type,
--   composed 
analyzeMoveOrderGraph
  :: Monoid t
  => ((Order Typical Move, [Order Typical Move], [Order Typical Support], [Order Typical Move]) -> t)
  -> (Cycle -> t)
  -> MoveOrderGraphWithSupports
  -> t
analyzeMoveOrderGraph ifPath ifCycle graph = case takeNext graph of
    Nothing -> mempty
    Just (taken, graph') -> case taken of
        Left x -> ifPath x `mappend` rest
        Right x -> ifCycle x `mappend` rest
      where rest = analyzeMoveOrderGraph ifPath ifCycle graph'

-- | In which the tip of a path and all competing moves and their supports
--   are resolved.
--
--   First component of argument is the tip of path, second is the competing
--   moves, third is all relevant supports, fourth is every unresolved
--   move in the graph but not in (order : competing).
--
analyzePath
  :: (Order Typical Move, [Order Typical Move], [Order Typical Support], [Order Typical Move])
  -> TypicalResolution
analyzePath (order, competing, supports, otherMoves) =
    TypicalResolution $ \(_, resolvedMoves) -> do
        -- Assumptions:
        -- (1) All (order : competing) have common target.
        -- (2) There is no unresolved move at this target.
        -- (3) All supports support one of the moves in (1).
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
        localResolvedSupports <- sequenceA $
            resolveLocalSupport otherMoves resolvedMoves <$> supports
        localResolvedMoves <- resolveLocalMoves (order : competing) localResolvedSupports
        return (localResolvedSupports, localResolvedMoves)

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
          -> MayFail (ResolutionError Typical) Identity [Resolved Order Typical Move]
        resolveLocalMoves moves resolvedSupports = passes . Identity $
            let supportedMoves = calculateSupport resolvedSupports <$> moves
                dominator = dominatingMove supportedMoves
            in  case dominator of
                    Nothing -> bounced moves <$> moves
                    Just x -> (x, Nothing) : (overpowered x <$> (moves \\ [x]))

        bounced :: [Order Typical Move] -> Order Typical Move -> Resolved Order Typical Move
        bounced moves move = (move, Just (MoveInsufficientSupport (moves \\ [move])))

        overpowered :: Order Typical Move -> Order Typical Move -> Resolved Order Typical Move
        overpowered dominator move = (move, Just (MoveOverpowered dominator))

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

        dominatingMove :: [(Int, Order Typical Move)] -> Maybe (Order Typical Move)
        dominatingMove xs = case xs of
            [] -> Nothing
            [x] -> Just (snd x)
            xs' -> snd . foldr dominates (0, Nothing) $ xs'
          where
            dominates (i, m) (j, l) =
                if i > j
                then (i, Just m)
                else if j > i
                then (j, l)
                else (i, Nothing)



-- | In which a simple cycle in a graph with no paths is resolved.
analyzeCycle :: Cycle -> TypicalResolution
analyzeCycle cycle =
    TypicalResolution $ \_ -> case cycle of
        -- In this case it's a 2-cycle; those always fail.
        Many x (One y) -> passes (Identity ([], [failX, failY]))
          where
            failX :: Resolved Order Typical Move
            failX = (x, Just (Move2Cycle y))
            failY :: Resolved Order Typical Move
            failY = (y, Just (Move2Cycle x))
        -- Any other cycle (holds included) succeeds.
        ncycle -> passes (Identity ([], succeeds <$> manyToList ncycle))

-- Whenever we pull the tip of a path we give every support which is
-- relevant. Same for each cycle. Then at the end we fail every remaining
-- support, safe in the knowledge that it did not have a corresponding move.
-- YES this is a good idea.
-- BUT note that we DO still need all of the UNRESOLVED orders in context, at
-- least to resolve supports. When we pull the tip of a path and all relevant
-- support, we can SAFELY resolve supports by looking at other offending moves,
-- safe in the knowledge that any of those offending moves is NOT a move which
-- must dislodge the support in order to break it, since if it were, we would
-- have resolved it already (it would have been tip of path)!
newtype TypicalResolution = TypicalResolution {
    runTypicalResolution
      :: ([Resolved Order Typical Support], [Resolved Order Typical Move])
      -> MayFail
           (ResolutionError Typical)
           Identity
           ([Resolved Order Typical Support], [Resolved Order Typical Move])
  }

makeTypicalResolution :: MoveOrderGraphWithSupports -> TypicalResolution
makeTypicalResolution = analyzeMoveOrderGraph analyzePath analyzeCycle

evalTypicalResolution
  :: TypicalResolution
  -> MayFail
       (ResolutionError Typical)
       Identity
       ([Resolved Order Typical Support], [Resolved Order Typical Move])
evalTypicalResolution = (flip runTypicalResolution) ([], [])

-- TODO prove it's a monoid.
instance Monoid TypicalResolution where
    mempty = TypicalResolution $ const (passes (Identity ([], [])))
    x `mappend` y = TypicalResolution $ \r -> do
        first <- runTypicalResolution x r
        second <- runTypicalResolution x first
        return $ up (++) second first
      where
        up :: (forall a . f a -> f a -> f a) -> (f a, f b) -> (f a, f b) -> (f a, f b)
        up f (x, y) (x', y') = (f x x', f y y')

-- | You must guarantee that there is at most one move in the list of orders
--   for each province!
makeMoveOrderGraph
  :: [Valid (Order Typical Move)]
  -> MoveOrderGraph
makeMoveOrderGraph =
    foldr addMove emptyMoveOrderGraph . fmap outValid

resolveTypicalOrders
  :: OrderResolution Typical [SomeResolved Order Typical]
resolveTypicalOrders validOrders =
    let moves = validMoves validOrders
        supports = outValid <$> validSupports validOrders
        moveGraphWithSupports = (makeMoveOrderGraph moves, supports)
        resolved = evalTypicalResolution (makeTypicalResolution (moveGraphWithSupports))
    in  concatenate <$> resolved
  where
    concatenate (resolvedSupports, resolvedMoves) =
        (SomeResolved <$> resolvedSupports) ++ (SomeResolved <$> resolvedMoves)

resolveTypical :: OrderResolution Typical [SomeResolved Order Typical]
resolveTypical =
        resolveMultipleOrdersAtProvince
    >=> resolveTypicalOrders




move1 :: Order Typical Move
move1 = Order (align ((Fleet, Normal Brest), MoveObject (Normal Brest)) France)

move2 :: Order Typical Move
move2 = Order (align ((Army, Normal Paris), MoveObject (Normal Brest)) France)

move3 :: Order Typical Move
move3 = Order (align ((Army, Normal Brest), MoveObject (Normal Paris)) France)

move4 :: Order Typical Move
move4 = Order (align ((Fleet, Normal EnglishChannel), MoveObject (Normal Brest)) France)

move5 :: Order Typical Move
move5 = Order (align ((Army, Normal Paris), MoveObject (Normal Picardy)) France)

move6 :: Order Typical Move
move6 = Order (align ((Fleet, Normal Picardy), MoveObject (Normal EnglishChannel)) France)

support1 :: Order Typical Support
support1 = Order (align ((Army, Normal Picardy), SupportObject (Army, Normal Paris) (Normal Brest)) France)
