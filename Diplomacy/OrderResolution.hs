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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Diplomacy.OrderResolution (

    Resolved
  , SomeResolved(..)
  , withSomeResolved
  , someResolvedValidOrder

  , FailureReason(..)

  , TypicalResolution
  , typicalResolution

  ) where

import Data.Typeable
import Data.Ord
import Data.List
import Data.Monoid
import Data.Either
import Data.Maybe
import Data.AtLeast
import Data.TypeNat.Nat
import Data.TypeNat.Vect
import Data.Functor.Identity
import Data.Traversable (sequenceA)
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad
import Control.Applicative
import Diplomacy.GreatPower
import Diplomacy.Aligned
import Diplomacy.Unit
import Diplomacy.Phase
import Diplomacy.Subject
import Diplomacy.OrderType
import Diplomacy.OrderObject
import Diplomacy.Order
import Diplomacy.Province
import Diplomacy.Zone
import Diplomacy.Valid
import Diplomacy.Subject
import Diplomacy.ProofSearch

import Debug.Trace

-- New plan: we express everything as natural deduction style judgements.
--
--   Move:
--
--     NoConvoyRoute orders m
--     ______________________
--     Fails orders m
--
--     SuccessfulConvoyRoutes orders m = nil
--     ________________________
--     Fails orders m
--
--     Dominated (RemoveIncomingMoves orders m) m
--     ________________________________________
--     Fails orders m
--
--     In2Cycle orders m m' -- implies m, m' are in orders!
--     SuccessfulConvoyRoutes orders m = nil
--     SuccessfulConvoyRoutes orders m' = nil
--     ________________________________________
--     Fails orders m
--
--     -- This one is dubious... needs to check the outgoing move as well I
--     -- believe, so let's (RemoveIncomingMoves orders m)
--     WouldDislodgeFriendly orders m
--     ______________________________
--     Fails orders m
--
--   Support:
--
--     Attacks m (OrderSubjectProvinceTarget s)
--     OrderSubjectProvinceTarget m /= SupportingInto s
--     ConvoyIndependent orders m s
--     ________________________________________________
--     Fails orders s
--
--     Attacks m (OrderSubjectProvinceTarget s)
--     OrderSubjectProvinceTarget m = SupportingInto s
--     Dislodges orders m (OrderSubject s)
--     ________________________________________
--     Fails orders s
--
--     NB in the immediately previous rule, we don't need to prove convoy
--     independence, because the support is attacking a subject from which
--     a MOVE order comes, therefore it does not attack a convoying fleet.
--
--   Convoy:
--
--     Dislodged orders c
--     ___________
--     Fails orders c
--
--   Surrender:
--
--   Withdraw:
--   
--     ConflictingWithdraws w w'
--     w' in orders
--     _________________________
--     Fails orders w
--
--   Disband:
--
--   Build:

type TypicalResolution = M.Map Zone (Aligned Unit, SomeResolved OrderObject Typical)

-- Left means assumed resolution, right means no resolution assumed.
type TypicalResolutionInput
    = M.Map Zone (Aligned Unit, Either (SomeResolved OrderObject Typical) (SomeOrderObject Typical))

-- We preserve the tagging of assumptions in the resolution output, to
-- facilitate the recursive "piling up" of assumptions.
type TypicalResolutionOutput
    = M.Map Zone (Aligned Unit, Either (SomeResolved OrderObject Typical) (SomeResolved OrderObject Typical))

-- Use an output as an input by dropping the resolutions of all non-assumptions.
preserveAssumptions :: TypicalResolutionOutput -> TypicalResolutionInput
preserveAssumptions = M.map makeInput
  where
    makeInput (aunit, x) = case x of
        Left y -> (aunit, Left y)
        Right (SomeResolved (x, _)) -> (aunit, Right $ SomeOrderObject x)

dropAssumptionTags :: TypicalResolutionOutput -> TypicalResolution
dropAssumptionTags = M.map dropTag
  where
    dropTag (aunit, x) = case x of
        Left y -> (aunit, y)
        Right y -> (aunit, y)

typicalResolutionAssuming
    :: TypicalResolutionInput
    -> TypicalResolutionOutput
typicalResolutionAssuming input =
    let resolution = M.mapWithKey (resolveOne resolution) input
    in  resolution
  where
    resolveOne
        :: TypicalResolutionOutput
        -> Zone
        -> (Aligned Unit, Either (SomeResolved OrderObject Typical) (SomeOrderObject Typical))
        -> (Aligned Unit, Either (SomeResolved OrderObject Typical) (SomeResolved OrderObject Typical))
    resolveOne resolution zone (aunit, x) = case x of
        Left y -> (aunit, Left y)
        Right y -> (aunit, Right (resolveSomeOrderTypical resolution zone (aunit, y)))

assumeNoOrder
    :: Zone
    -> TypicalResolutionInput
    -> TypicalResolutionInput
assumeNoOrder = M.alter (const Nothing)

assumeSucceeds
    :: Zone
    -> TypicalResolutionInput
    -> TypicalResolutionInput
assumeSucceeds zone = M.adjust makeSucceeds zone
  where
    makeSucceeds
        :: (Aligned Unit, Either (SomeResolved OrderObject Typical) (SomeOrderObject Typical))
        -> (Aligned Unit, Either (SomeResolved OrderObject Typical) (SomeOrderObject Typical))
    makeSucceeds (aunit, x) = case x of
        Left (SomeResolved (x, _)) -> (aunit, Left (SomeResolved (x, Nothing)))
        Right (SomeOrderObject x) -> (aunit, Left (SomeResolved (x, Nothing)))

noAssumptions
    :: M.Map Zone (Aligned Unit, SomeOrderObject Typical)
    -> TypicalResolutionInput
noAssumptions = M.map (\(x, y) -> (x, Right y))

typicalResolution
    :: M.Map Zone (Aligned Unit, SomeOrderObject Typical)
    -> TypicalResolution
typicalResolution = dropAssumptionTags . typicalResolutionAssuming . noAssumptions

data RequiresConvoy
    = RequiresConvoy
    | DoesNotRequireConvoy
    deriving (Show)

type ConvoyRoute = [(Zone, Maybe (Aligned Subject))]

data ConvoyRoutes = ConvoyRoutes {
      convoyRoutesParadox :: [ConvoyRoute]
    , convoyRoutesNonParadox :: [ConvoyRoute]
    }
    deriving (Show)

-- | Any move between non-adjacent provinces is deemed to require a
--   convoy, even if both provinces are inland. Order validation rules
--   out those cases though.
moveRequiresConvoy :: ProvinceTarget -> ProvinceTarget -> Bool
moveRequiresConvoy ptFrom ptTo = not (isSameOrAdjacent movingTo movingFrom)
  where
    movingTo = ptProvince ptFrom
    movingFrom = ptProvince ptTo

isConvoyMoveWithNoConvoyRoute :: MoveClassification -> Bool
isConvoyMoveWithNoConvoyRoute thisClassification = case thisClassification of
    NotHold RequiresConvoy theseConvoyRoutes _ _ -> null (successfulConvoyRoutes theseConvoyRoutes)
    _ -> False

-- | Description of an order's support (that order is unfortunately not a part
--   of this type or its values). Each entry in the list means a unit belonging
--   to some power at some place supports that implicit order.
type Supports = [Aligned Subject]

-- | Given a Subject and a ProvinceTarget, meaning Subject attempting to move to
--   that ProvinceTarget (or support/convoy/hold in case it's the same as the
--   Subject's), calculate the supporters of that order.
support :: TypicalResolutionOutput -> Subject -> ProvinceTarget -> Supports
support resolution subject goingTo = M.foldWithKey selector [] (dropAssumptionTags resolution)
  where
    selector
        :: Zone
        -> (Aligned Unit, SomeResolved OrderObject Typical)
        -> [Aligned Subject]
        -> [Aligned Subject]
    selector zone (aunit, SomeResolved (object, thisResolution)) b = case object of
        SupportObject supportSubject supportTo ->
            if    supportSubject /= subject
               || supportTo /= goingTo
            then b
            else case thisResolution of
                Nothing -> align (alignedThing aunit, zoneProvinceTarget zone) (alignedGreatPower aunit) : b
                _ -> b
        _ -> b

foreignSupport
    :: TypicalResolutionOutput
    -> GreatPower
    -> Subject
    -> ProvinceTarget
    -> Supports
foreignSupport resolution power subject goingTo =
    filter isForeignSupport (support resolution subject goingTo)
  where
    isForeignSupport asubj = alignedGreatPower asubj /= power

-- TODO should be able to do this with only the classification, no? The issue
-- is that the classification doesn't contain the zone or great power for which
-- it's relevant :(
isMoveDislodgedFromAttackedZone
    :: TypicalResolutionOutput
    -> Zone
    -> (Aligned Unit, OrderObject Typical Move)
    -> Bool
isMoveDislodgedFromAttackedZone resolution zoneFrom (aunit, object) = case thisClassification of
    Hold _ -> False
    NotHold _ _ _ thisIncumbant -> case thisIncumbant of
        -- How to decide this? It strikes me as a little complex...
        -- It must be
        --
        --   1. a foreign order (no self-dislodge).
        --   2. a have more foreign support than this order.
        --
        -- Should abstract this later, as I'm sure it will come up
        -- again!
        ComplementaryMove WouldSucceed asubj target ->
            let opposingSupports = foreignSupport resolution (alignedGreatPower aunit) (alignedThing asubj) target
                thisSupports = support resolution (alignedThing aunit, zoneProvinceTarget zoneFrom) (zoneProvinceTarget zoneTo)
            in     alignedGreatPower aunit /= alignedGreatPower asubj
                && length opposingSupports > length thisSupports
        _ -> False
  where
    thisClassification = classify resolution zoneFrom (aunit, object)
    zoneTo = Zone (moveTarget object)


-- | Relative to a Zone (given only by context, unfortunately). Each entry means
--   there is a move from that zone by that unit belonging to that great power
--   against the implicit Zone.
type CompetingMoves = [(Aligned Subject, ProvinceTarget)]

-- | Get the competing moves (enough information to reconstruct them) against
--   a move from one zone to another. Yes, they're only moves; a hold, support,
--   or convoy at the target zone is not included.
competingMoves
    :: TypicalResolutionOutput
    -> Zone
    -> Zone
    -> CompetingMoves
competingMoves resolution zoneFrom zoneTo = M.foldWithKey selector [] (dropAssumptionTags resolution')
  where
    -- It is ESSENTIAL that we forget about the order at THIS zone when we
    -- compute the competing moves. If we don't, the program may not terminate.
    -- For example:
    --
    --   1. F North Sea -> Holland
    --   2. F Holland -> North Sea
    --   3. F Norwegian Sea -> North Sea
    --   4. F Ruhr -> Holland
    --
    -- To compute the competing moves for 4, we must classify 1 to get the
    -- incumbant, so we must resolve 2, which requires classifying 3, which
    -- in turn demands that we resolve 1, of which 4 is a competing move!
    resolution' = M.delete zoneFrom resolution
    selector
        :: Zone
        -> (Aligned Unit, SomeResolved OrderObject Typical)
        -> CompetingMoves
        -> CompetingMoves
    selector zone (aunit, SomeResolved (object, _)) b = case object of
        MoveObject movingTo ->
            if    zone == zoneFrom
               || Zone movingTo /= zoneTo
               || isConvoyMoveWithNoConvoyRoute thisClassification
               -- A dislodged unit cannot cause a standoff in the province
               -- from which it was dislodged.
               || isMoveDislodgedFromAttackedZone resolution' zone (aunit, object)
            then b
            else let subject = (alignedThing aunit, zoneProvinceTarget zone)
                     asubject = align subject (alignedGreatPower aunit)
                 in  (asubject, movingTo) : b
          where
            thisClassification = classify resolution' zone (aunit, object)
        _ -> b

data WouldSucceed
    = WouldSucceed
    | WouldNotSucceed
    deriving (Show)

data Incumbant
    = ComplementaryMove WouldSucceed (Aligned Subject) ProvinceTarget
    -- ^ Only if the move succeeds in the absence of its complement.
    --   The ProvinceTarget in the subject is from where the complement moves,
    --   and the other ProvinceTarget is to where the complementary move
    --   wishes to go. These are necessary due to the coarseness of Zone
    --   Eq.
    --
    --   This notion is useful because in the case of complementary moves,
    --   support of both moves must be compared against each-other, as though
    --   one unit must advance through the opposite advance of the other.
    --   Compare at returning moves, in which the returning unit cannot have
    --   any support for its return.
    | ReturningMove (Aligned Subject) ProvinceTarget
    -- ^ Only if the move fails (could be complementary).
    | Stationary (Aligned Subject)
    -- ^ Here we give a subject because the ProvinceTarget is NOT implicit.
    --   For instance, if we know that Zone (Special SpainSouth) is stationary,
    --   we don't know whether that thing is stationary at
    --       Special SpainSouth
    --       Special SpainNorth
    --       Normal Spain
    --   It could be any of these.
    | NoIncumbant
    deriving (Show)

-- | Lookup a key in a map and get back the actual key as well. Useful when
--   the key Eq instance is not quite so sharp.
lookupWithKey
    :: Ord k
    => k
    -> M.Map k v
    -> Maybe (k, v)
lookupWithKey k m =
    let v = M.lookup k m
        keys = M.keysSet m
        -- keys `S.intersection` S.singleton k is empty iff v is Nothing, so
        -- this won't be undefined.
        k' = Data.List.head (S.elems (keys `S.intersection` S.singleton k))
    in fmap (\x -> (k', x)) v

incumbant
    :: TypicalResolutionOutput
    -> Zone
    -> Zone
    -> Incumbant
incumbant resolution zoneFrom zoneTo = case lookupWithKey zoneTo resolution' of
    -- We lookupWithKey because the actual ProvinceTarget where the incumbant
    -- lies may not be ProvinceTarget-equal with the ProvinceTarget in the
    -- Zone which we used to index the map!
    Just (zoneTo', (aunit, SomeResolved (object, res))) -> case object of
        MoveObject pt ->
            if Zone pt == zoneTo
            then Stationary (align (alignedThing aunit, zoneProvinceTarget zoneTo') (alignedGreatPower aunit))
            else if Zone pt == zoneFrom
            -- It's a move back against zoneFrom. If it succeeds (in the absence
            -- of any move at zoneFrom) then we call it complementary; the
            -- actual resolution of the move at zoneFrom may change this
            -- outcome! If it fails, we'll just treat it like a returning move.
            then case res of
                Nothing -> ComplementaryMove WouldSucceed (align (alignedThing aunit, zoneProvinceTarget zoneTo') (alignedGreatPower aunit)) pt
                Just _ -> ComplementaryMove WouldNotSucceed (align (alignedThing aunit, zoneProvinceTarget zoneTo') (alignedGreatPower aunit)) pt
            else case res of
                Nothing -> NoIncumbant
                Just _ -> ReturningMove (align (alignedThing aunit, pt) (alignedGreatPower aunit)) (zoneProvinceTarget zoneTo')
        _ -> Stationary (align (alignedThing aunit, zoneProvinceTarget zoneTo') (alignedGreatPower aunit))
    _ -> NoIncumbant
  where
    resolutionThisSucceeds = typicalResolutionAssuming (assumeSucceeds zoneFrom (preserveAssumptions resolution))
    resolution' = dropAssumptionTags resolutionThisSucceeds

data MoveClassification
    = Hold CompetingMoves
    | NotHold RequiresConvoy ConvoyRoutes CompetingMoves Incumbant
    deriving (Show)

classify
    :: TypicalResolutionOutput
    -> Zone
    -> (Aligned Unit, OrderObject Typical Move)
    -> MoveClassification
classify resolution zone (aunit, MoveObject movingTo) =
    if zone == Zone movingTo
    then Hold (holdCompetingMoves resolution zone (Zone movingTo))
    else let power = alignedGreatPower aunit
             unit = alignedThing aunit
             pt = zoneProvinceTarget zone
             asubject = align (unit, pt) power
         in  classifyNonHold resolution asubject movingTo
  where

    -- TBD should we here calculate supports of the competing move, using the
    -- alignment to eliminate non-foreign support and non-foreign moves?!
    -- Yeah, why not?
    -- In non hold we would do this for competing moves, but for the incumbant
    -- if there is one.
    holdCompetingMoves
        :: TypicalResolutionOutput
        -> Zone
        -> Zone
        -> CompetingMoves
    holdCompetingMoves resolution zoneFrom zoneTo = theseCompetingMoves
      where
        theseCompetingMoves = competingMoves resolution zoneFrom zoneTo

    classifyNonHold
        :: TypicalResolutionOutput
        -> Aligned Subject
        -> ProvinceTarget
        -> MoveClassification
    classifyNonHold resolution asubject pt =
        NotHold thisRequiresConvoy theseConvoyRoutes theseCompetingMoves thisIncumbant
      where
        thisRequiresConvoy =
            if moveRequiresConvoy (zoneProvinceTarget zoneFrom) (zoneProvinceTarget zoneTo)
            then RequiresConvoy
            else DoesNotRequireConvoy
        theseConvoyRoutes = convoyRoutes resolution (alignedThing asubject) pt 
        -- TODO Tuesday: compute the competing moves, here and in the
        -- Hold case. This will involve gathering them from the resolution,
        -- classifying them, and using the convoy routes and incumbant fields
        -- in order to determine whether they take part in the list (no
        -- convoy routes but requies a convoy means it's out; a complementary
        -- incumbant which dislodges it means it's out)
        theseCompetingMoves = competingMoves resolution zoneFrom zoneTo
        thisIncumbant = incumbant resolution zoneFrom zoneTo
        zoneFrom = Zone (subjectProvinceTarget (alignedThing asubject))
        zoneTo = Zone pt

-- | All convoy routes which connect the subject to the given ProvinceTarget.
--   Each element of a route gives its zone (zone of the convoying fleet which
--   composese the route) as well as an indication of whether it was
--   dislodged (Just means it was dislodged by that subject).
rawConvoyRoutes
    :: TypicalResolutionOutput
    -> Subject
    -> ProvinceTarget
    -> [ConvoyRoute]
rawConvoyRoutes resolution' (unit, ptFrom) ptTo = do
    let pool = viableConvoyZones
    let starters = filter isStarter pool
    s <- starters
    let routes = paths pool ptTo s
    (fmap . fmap) tagWithChange routes

  where

    resolution = dropAssumptionTags resolution'

    tagWithChange :: Zone -> (Zone, Maybe (Aligned Subject))
    tagWithChange zone = (zone, change resolution zone)

    viableConvoyZones :: [Zone]
    viableConvoyZones = M.keys (M.filter isViableConvoy resolution)

    isViableConvoy
        :: (Aligned Unit, SomeResolved OrderObject Typical)
        -> Bool
    isViableConvoy (aunit, SomeResolved (object, _)) = case object of
        ConvoyObject (unit', convoyingFrom) convoyingTo ->
               unit == unit'
            && ptFrom == convoyingFrom
            && ptTo == convoyingTo
        _ -> False

    isStarter :: Zone -> Bool
    isStarter zone = adjacent (ptProvince (zoneProvinceTarget zone)) (ptProvince ptFrom)

    paths
        :: [Zone]
        -> ProvinceTarget
        -> Zone
        -> [[Zone]]
    paths pool target zone =
        case adjacent (ptProvince (zoneProvinceTarget zone)) (ptProvince target) of
            True -> [[zone]]
            False -> do
                let shrunkenPool = pool \\ [zone]
                let neighbourZones = fmap Zone (neighbours (zoneProvinceTarget zone))
                n <- neighbourZones `intersect` shrunkenPool
                fmap ((:) zone) (paths shrunkenPool target n)

convoyRoutes
    :: TypicalResolutionOutput
    -> Subject
    -> ProvinceTarget
    -> ConvoyRoutes
convoyRoutes resolution subject pt =
    let routes = rawConvoyRoutes resolution subject pt
        (paradox, nonParadox) = partition (isParadoxRoute resolution pt) routes
    in  ConvoyRoutes paradox nonParadox

-- | Identify convoy routes which are paradox-inducing; those routes whose
--   success is contingent upon the success of the move which they convoy!
isParadoxRoute
    :: TypicalResolutionOutput
    -> ProvinceTarget -- ^ The destination of the route.
    -> [(Zone, Maybe (Aligned Subject))]
    -> Bool
isParadoxRoute resolution destination =
    any (\(z, _) -> Just z == paradoxInducingConvoyZone resolution (Zone destination))

-- | Initial characterization of a support order which cannot be cut by a
--   convoyed move to the given Zone. That's to say, if there is any such
--   support, it will turn up in this. However it must also support an attack
--   against a convoying fleet in some route.
paradoxInducingSupport
    :: TypicalResolutionOutput
    -> Zone -- ^ The destination of a convoy route.
    -> Maybe (OrderObject Typical Support)
paradoxInducingSupport resolution zone =
    case M.lookup zone (dropAssumptionTags resolution) of
        Just (aunit, SomeResolved (s@(SupportObject _ _), _)) -> Just s
        _ -> Nothing

-- | If Just, then any convoy route which includes this Zone is a paradox route
paradoxInducingConvoyZone
    :: TypicalResolutionOutput
    -> Zone -- ^ Destination of convoy.
    -> Maybe Zone
paradoxInducingConvoyZone resolution =
    fmap (Zone . supportTarget) . paradoxInducingSupport resolution

-- | These are always non-paradox routes.
successfulConvoyRoutes :: ConvoyRoutes -> [ConvoyRoute]
successfulConvoyRoutes =
    filter isSuccessful . convoyRoutesNonParadox
  where
    isSuccessful = all (isNothing . snd)

resolveSomeOrderTypical
    :: TypicalResolutionOutput
    -> Zone
    -> (Aligned Unit, SomeOrderObject Typical)
    -> SomeResolved OrderObject Typical
resolveSomeOrderTypical resolution zone (aunit, SomeOrderObject object) =

    let thisResolution :: SomeResolved OrderObject Typical
        thisResolution = case object of
            MoveObject _ -> SomeResolved (object, resolveMove object)
            SupportObject _ _ -> SomeResolved (object, resolveSupport object)
            ConvoyObject _ _ -> SomeResolved (object, resolveConvoy object)

        -- ****
        -- MOVE
        -- ****
        --
        -- There are _ reasons to fail a move:
        --
        --   MoveNoConvoy : the move requires a convoy, there is no paradox
        --   convoy route, and there's no suitable path of successful convoys
        --   for it.
        --   MoveConvoyParadox : the move requires a convoy, there is a paradox
        --   convoy route, and all non-paradox convoy routes fail.
        --   MoveOverpowered : there is some other move of strictly greater
        --   support into this move's target.
        --   MoveBounced : this move is not a hold, and it is not a dominator
        --   at its target.
        --   MoveFriendlyDislodge : the move would dislodge a friendly unit.
        resolveMove :: OrderObject Typical Move -> Maybe (FailureReason Typical Move)
        resolveMove moveObject = case classify resolution zone (aunit, moveObject) of

            -- A hold is easy: it fails iff there is a foreign move with more
            -- foreign support than it.
            Hold theseCompetingMoves -> case dominator of
                Nothing -> Nothing
                Just (x, ss) ->
                    if length ss <= length thisSupports
                    then Nothing
                    -- TBD HoldOverpowered failure reason?
                    else Just (MoveOverpowered (AtLeast (VCons x VNil) []))
              where
                dominator = case sortedOpposingSupports of
                    [] -> Nothing
                    [x] -> Just x
                    x : y : _ -> if length (snd x) > length (snd y)
                                 then Just x
                                 else Nothing
                sortedOpposingSupports = sortBy comparator opposingSupports
                comparator :: (Aligned Subject, Supports) -> (Aligned Subject, Supports) -> Ordering
                comparator (_, xs) (_, ys) = Down (length xs) `compare` Down (length ys)
                opposingSupports :: [(Aligned Subject, Supports)]
                opposingSupports = fmap (\x -> (fst x, calculateOpposingSupports x)) foreignCompetingMoves
                calculateOpposingSupports :: (Aligned Subject, ProvinceTarget) -> Supports
                calculateOpposingSupports (asubj, pt) = foreignSupport resolution (alignedGreatPower aunit) (alignedThing asubj) pt
                foreignCompetingMoves :: CompetingMoves
                foreignCompetingMoves = filter (\(asubj, _) -> alignedGreatPower asubj /= alignedGreatPower aunit) theseCompetingMoves
                thisSupports :: Supports
                thisSupports = support resolution (alignedThing aunit, zoneProvinceTarget zone) (zoneProvinceTarget zone)


            -- For a non hold:
            --
            --   1. check if it doesn't have the required convoy.
            --   2. check if it bounces off/is overpowered by the competing moves.
            --   3. check if it bounces off/is overpowered by the incumbant.
            NotHold requiresConvoy theseConvoyRoutes theseCompetingMoves thisIncumbant ->
                case (checkConvoy, checkCompeting, checkIncumbant) of
                    -- We play with the order here, so that a move overpowered
                    -- is always preferred over a move bounced.
                    (Nothing, x@(Just (MoveBounced _)), y@(Just (MoveOverpowered _))) -> y
                    (Nothing, x@(Just (MoveBounced _)), y@(Just (MoveBounced _))) -> y
                    (Nothing, x@(Just (MoveOverpowered _)), y@(Just (MoveBounced _))) -> x
                    (Nothing, x@(Just (MoveOverpowered _)), y@(Just (MoveOverpowered _))) -> y
                    (x, y, z) -> x <|> y <|> z
              where

                checkConvoy = case requiresConvoy of
                    RequiresConvoy ->
                        if    null (successfulConvoyRoutes theseConvoyRoutes)
                        then if null (convoyRoutesParadox theseConvoyRoutes)
                             then Just MoveNoConvoy
                             else Just MoveConvoyParadox
                        else Nothing
                    _ -> Nothing

                -- For competing moves here, we don't care about foriegn orders
                -- or supports, it's all the same.
                checkCompeting = case sortedOpposingSupports of
                    [] -> Nothing
                    ((x, ss) : xs) ->
                        if length ss == length thisSupports
                        then Just (MoveBounced (AtLeast (VCons x VNil) equallySupported))
                        else if length ss > length thisSupports
                        then Just (MoveOverpowered (AtLeast (VCons x VNil) equallySupported))
                        else Nothing
                      where
                        equallySupported = fmap fst (filter (\(x, ss') -> length ss' == length ss) xs)
                  where
                    sortedOpposingSupports = sortBy comparator opposingSupports
                    comparator :: (Aligned Subject, Supports) -> (Aligned Subject, Supports) -> Ordering
                    comparator (_, xs) (_, ys) = Down (length xs) `compare` Down (length ys)
                    opposingSupports :: [(Aligned Subject, Supports)]
                    opposingSupports = fmap (\x -> (fst x, calculateOpposingSupports x)) theseCompetingMoves
                    calculateOpposingSupports :: (Aligned Subject, ProvinceTarget) -> Supports
                    calculateOpposingSupports (asubj, pt) = support resolution (alignedThing asubj) pt
                    thisSupports :: Supports
                    thisSupports = support resolution (alignedThing aunit, zoneProvinceTarget zone) (moveTarget moveObject)


                checkIncumbant = case thisIncumbant of

                    NoIncumbant -> Nothing

                    -- Stationary: fail if this move (which threatens to
                    -- dislodge the stationary unit) does not dominate the
                    -- zone WITHOUT support from the stationary unit's great
                    -- power, or if that unit is not foreign
                    -- (MoveFiendlyDislodge).
                    Stationary asubj -> case sortedOpposingSupports of
                        [] -> Nothing -- Actually impossible.
                        ((x, ss) : xs) ->
                            if length ss == length thisSupports
                            then Just (MoveBounced (AtLeast (VCons x VNil) equallySupported))
                            else if length ss > length thisSupports
                            then Just (MoveOverpowered (AtLeast (VCons x VNil) equallySupported))
                            else if opposingPower == thisPower
                            then Just (MoveFriendlyDislodge (alignedThing aunit))
                            else Nothing
                          where
                            equallySupported = fmap fst (filter (\(x, ss') -> length ss' == length ss) xs)
                      where
                        thisSupports :: Supports
                        thisSupports = foreignSupport resolution opposingPower (alignedThing aunit, zoneProvinceTarget zone) (moveTarget moveObject)
                        sortedOpposingSupports = sortBy comparator opposingSupports
                        comparator :: (Aligned Subject, Supports) -> (Aligned Subject, Supports) -> Ordering
                        comparator (_, xs) (_, ys) = Down (length xs) `compare` Down (length ys)
                        opposingSupports :: [(Aligned Subject, Supports)]
                        opposingSupports = fmap (\x -> (fst x, calculateOpposingSupports x)) theseCompetingMovesWithStationary
                        calculateOpposingSupports :: (Aligned Subject, ProvinceTarget) -> Supports
                        calculateOpposingSupports (asubj, pt) = support resolution (alignedThing asubj) pt
                        theseCompetingMovesWithStationary = (asubj, subjectProvinceTarget thisSubject) : theseCompetingMoves
                        opposingSubject = alignedThing asubj
                        opposingPower = alignedGreatPower asubj
                        thisPower = alignedGreatPower aunit
                        thisSubject = alignedThing asubj

                    -- Returning: fail if this move (which threatens to
                    -- dislodge the returning unit) does not dominate the
                    -- zone WITHOUT support from the returning unit's great
                    -- power, or if that unit is not foreign
                    -- (MoveFiendlyDislodge).
                    ReturningMove asubj pt -> case sortedOpposingSupports of
                        [] -> Nothing -- Actually impossible
                        ((x, ss) : xs) ->
                            if length ss == length thisSupports
                            then Just (MoveBounced (AtLeast (VCons x VNil) equallySupported))
                            else if length ss > length thisSupports
                            then Just (MoveOverpowered (AtLeast (VCons x VNil) equallySupported))
                            else if opposingPower == thisPower
                            then Just (MoveFriendlyDislodge (subjectUnit (alignedThing asubj)))
                            else Nothing
                          where
                            equallySupported = fmap fst (filter (\(x, ss') -> length ss' == length ss) xs)
                      where
                        thisSupports :: Supports
                        thisSupports = foreignSupport resolution (alignedGreatPower asubj) (alignedThing aunit, zoneProvinceTarget zone) (moveTarget moveObject)
                        -- We add the returning move with no supports, making
                        -- it look like it was a hold, so that if a move bounces
                        -- off a returning move, it's indicated by the origin
                        -- of the returning move, rather than its destination.
                        sortedOpposingSupports = sortBy comparator ((align (opposingUnit, pt) opposingPower, []) : opposingSupports)
                        comparator :: (Aligned Subject, Supports) -> (Aligned Subject, Supports) -> Ordering
                        comparator (_, xs) (_, ys) = Down (length xs) `compare` Down (length ys)
                        opposingSupports :: [(Aligned Subject, Supports)]
                        opposingSupports = fmap (\x -> (fst x, calculateOpposingSupports x)) theseCompetingMoves
                        calculateOpposingSupports :: (Aligned Subject, ProvinceTarget) -> Supports
                        calculateOpposingSupports (asubj, pt) = support resolution (alignedThing asubj) pt
                        opposingSubject = alignedThing asubj
                        opposingUnit = subjectUnit opposingSubject
                        opposingPower = alignedGreatPower asubj
                        thisPower = alignedGreatPower aunit

                    -- Complementary where the other would not succeed even
                    -- without this one.
                    -- HERE AS WELL we must check that without supports friendly
                    -- to the complementary, this move would still dominate!
                    ComplementaryMove WouldNotSucceed asubj target -> case sortedOpposingSupports of
                        [] -> Nothing -- Impossible
                        ((x, ss) : xs) ->
                            if length ss > length thisSupports && opposingPower /= thisPower
                            then Just (MoveOverpowered (AtLeast (VCons x VNil) equallySupported))
                            else if length thisSupports > length ss && opposingPower == thisPower
                            then Just (MoveFriendlyDislodge opposingUnit)
                            else if length ss == length thisSupports
                            then Just (MoveBounced (AtLeast (VCons x VNil) equallySupported))
                            else Nothing
                          where
                            equallySupported = fmap fst (filter (\(x, ss') -> length ss' == length ss) xs)
                      where
                        sortedOpposingSupports = sortBy comparator ((asubj, complementarySupports) : opposingSupports)
                        comparator :: (Aligned Subject, Supports) -> (Aligned Subject, Supports) -> Ordering
                        comparator (_, xs) (_, ys) = Down (length xs) `compare` Down (length ys)
                        opposingSupports :: [(Aligned Subject, Supports)]
                        opposingSupports = fmap (\x -> (fst x, calculateOpposingSupports x)) theseCompetingMoves
                        calculateOpposingSupports :: (Aligned Subject, ProvinceTarget) -> Supports
                        calculateOpposingSupports (asubj, pt) = support resolution (alignedThing asubj) pt
                        complementarySupports :: Supports
                        complementarySupports = foreignSupport resolution thisPower opposingSubject target
                        thisSupports :: Supports
                        thisSupports = foreignSupport resolution opposingPower (alignedThing aunit, zoneProvinceTarget zone) (moveTarget moveObject)
                        opposingPower = alignedGreatPower asubj
                        opposingSubject = alignedThing asubj
                        opposingUnit = subjectUnit opposingSubject
                        thisPower = alignedGreatPower aunit


                    -- Complementary where the other would succeed without
                    -- this one.
                    -- HERE AS WELL we must check that without supports friendly
                    -- to the complementary, this move would still dominate!
                    -- However, since the other move would succeed, it's enough
                    -- to know that at least one of these moves (this or the
                    -- complementary) has at least one successful convoy route
                    -- in order to pass this test.
                    ComplementaryMove WouldSucceed asubj target ->
                        if     not (null opposingSuccessfulConvoyRoutes)
                            || not (null thisSuccessfulConvoyRoutes)
                        then Nothing
                        else case sortedOpposingSupports of
                            [] -> Nothing -- Impossible
                            ((x, ss) : xs) ->
                                if length ss > length thisSupports && opposingPower /= thisPower
                                then Just (MoveOverpowered (AtLeast (VCons x VNil) equallySupported))
                                else if length thisSupports > length ss && opposingPower == thisPower
                                then Just (MoveFriendlyDislodge opposingUnit)
                                else if    length ss == length thisSupports
                                then Just (MoveBounced (AtLeast (VCons x VNil) equallySupported))
                                else Nothing
                              where
                                equallySupported = fmap fst (filter (\(x, ss') -> length ss' == length ss) xs)
                      where
                        sortedOpposingSupports = sortBy comparator ((asubj, complementarySupports) : opposingSupports)
                        comparator :: (Aligned Subject, Supports) -> (Aligned Subject, Supports) -> Ordering
                        comparator (_, xs) (_, ys) = Down (length xs) `compare` Down (length ys)
                        opposingSupports :: [(Aligned Subject, Supports)]
                        opposingSupports = fmap (\x -> (fst x, calculateOpposingSupports x)) theseCompetingMoves
                        calculateOpposingSupports :: (Aligned Subject, ProvinceTarget) -> Supports
                        calculateOpposingSupports (asubj, pt) = support resolution (alignedThing asubj) pt
                        complementarySupports :: Supports
                        complementarySupports = foreignSupport resolution thisPower opposingSubject target
                        thisSupports :: Supports
                        thisSupports = foreignSupport resolution opposingPower (alignedThing aunit, zoneProvinceTarget zone) (moveTarget moveObject)
                        opposingSuccessfulConvoyRoutes :: [ConvoyRoute]
                        opposingSuccessfulConvoyRoutes = successfulConvoyRoutes opposingConvoyRoutes
                        thisSuccessfulConvoyRoutes :: [ConvoyRoute]
                        thisSuccessfulConvoyRoutes = successfulConvoyRoutes theseConvoyRoutes
                        opposingConvoyRoutes :: ConvoyRoutes
                        opposingConvoyRoutes = convoyRoutes resolution opposingSubject target
                        opposingPower = alignedGreatPower asubj
                        opposingSubject = alignedThing asubj
                        opposingUnit = subjectUnit opposingSubject
                        thisPower = alignedGreatPower aunit

        -- *******
        -- SUPPORT
        -- *******
        --
        -- There are three reasons to fail a support:
        --
        --   SupportVoid : the complementary order was not given.
        --   For instance, F Eng S F MAt -> Bre cannot succeed unless
        --   some great power issues F MAt -> Bre. Similarly,
        --   F Eng S F MAt -> MAt cannot success unless some great power
        --   issues F MAt Hold OR F MAt S <anything> OR F MAt C <anything>.
        resolveSupport
            :: OrderObject Typical Support
            -> Maybe (FailureReason Typical Support)
        resolveSupport supportObject =
                supportVoid supportObject
            <|> supportCut supportObject
            <|> supportDislodged supportObject

        -- A support is Void if the supported order was not given.
        supportVoid
            :: OrderObject Typical Support
            -> Maybe (FailureReason Typical Support)
        supportVoid (SupportObject supportingSubject supportingTo) =
            case M.lookup supportingFrom (dropAssumptionTags resolution) of
                Nothing -> Just SupportVoid
                Just (aunit, SomeResolved (object, _)) ->
                    if    supportingUnit == alignedThing aunit
                       && supportingTo == destination
                    then Nothing
                    else Just SupportVoid
                  where
                    destination = case object of
                        MoveObject pt -> pt
                        _ -> zoneProvinceTarget supportingFrom

          where

            supportingFrom :: Zone
            supportingFrom = Zone (snd supportingSubject)

            supportingUnit :: Unit
            supportingUnit = fst supportingSubject


        -- Support is cut if there is a move into its territory issued by
        -- another great power, from a territory other than the one into which
        -- support is directed. If that move requires a convoy, then there must
        -- be at least one successful convoy route. To avoid nontermination
        -- which would arise from the classic convoy paradox:
        --
        --   France: Army Brest -> English Channel -> London.
        --   France: Fleet English Channel CONVOY Army Brest -> London. 
        --
        --   England: Fleet London SUPPORT Fleet Wales -> English Channel.
        --   England: Fleet Wales -> English Channel. 
        --
        -- we use the notion of convoy-independence. In this example, we would
        -- check whether the convoy route succeeds, which in-turn check whether
        -- the English move succeeds, which would ask whether the English
        -- support succeeds, which would in-turn ask whether the French
        -- move has a successful convoy route, and so on...
        --
        -- We could cut the loop by inspecting only the independent convoy
        -- routes, those routes such that their convoying fleets are not
        -- attacked by a move which is supported by this support. The next rule,
        -- convoyDislodged, is sensitive to this, because under this paradox
        -- resolution, it's possible for a dislodged unit to give support, i.e.
        -- when it was dislodged by a move which did not cut it.
        --
        -- Another option is to identify the moves which participate in these
        -- paradoxes and fail them (MoveConvoyParadox). But how does this hold
        -- up in case there's more than one convoy route? Aha, yes we would
        -- first have to ensure that none of the other routes are successful,
        -- and only then could we say it's MoveConvoyParadox. So, this amounts
        -- to 1. grabbing all convoy routes 2. isolating any paradoxical ones
        -- 3. checking whether any nonparadoxical one succeeds. Then
        --
        --     no successful nonparadoxical, at least one paradoxical -> MoveConvoyParadox
        --     no successful nonparadoxical, no paradoxical -> MoveNoConvoy
        --     successful nonparadoxical, _ -> Succeeds
        --
        -- Both of these strategies are explained here:
        -- http://diplom.org/Zine/F1999R/Debate/resolve.cgi
        supportCut
            :: OrderObject Typical Support
            -> Maybe (FailureReason Typical Support)
        supportCut (SupportObject supportingSubject supportingTo) =
            case filter issuedByOtherGreatPower offendingMoves of
                [] -> Nothing
                x : xs -> Just (SupportCut (AtLeast (VCons x VNil) xs))

          where

            issuedByOtherGreatPower :: Aligned Subject -> Bool
            issuedByOtherGreatPower x = alignedGreatPower aunit /= alignedGreatPower x

            supportingFrom :: Zone
            supportingFrom = zone

            offendingMoves :: [Aligned Subject]
            offendingMoves = M.elems (M.mapMaybeWithKey pickOffendingMove (dropAssumptionTags resolution))

            pickOffendingMove
                :: Zone
                -> (Aligned Unit, SomeResolved OrderObject Typical)
                -> Maybe (Aligned Subject)
            pickOffendingMove zone (aunit', SomeResolved (object, _)) =
                case object of
                    MoveObject movingTo ->
                        if    Zone movingTo == supportingFrom
                           && Zone supportingTo /= zone
                           && not (isConvoyMoveWithNoConvoyRoute thisClassification)
                        then Just $ align (alignedThing aunit', zoneProvinceTarget zone) (alignedGreatPower aunit')
                        else Nothing
                      where
                        thisClassification = classify resolution zone (aunit', object)
                    _ -> Nothing

        supportDislodged
            :: OrderObject Typical Support
            -> Maybe (FailureReason Typical Support)
        supportDislodged _ = case change (dropAssumptionTags resolution) zone of
            Nothing -> Nothing
            Just dislodger -> Just (SupportDislodged dislodger)

        -- ******
        -- CONVOY
        -- ******
        --
        -- There are two reasons to fail a convoy:
        --
        --   ConvoyVoid : the complementary move order was not given.
        --   For instance, F Eng C A Bre -> Wal cannot succeed unless some
        --   great power issues A Bre -> Wal. If no such order is issued, we
        --   say that the convoy order is void.
        --
        --   ConvoyNoRoute : there is no route of undisrupted convoy orders
        --   from the convoy source to convoy terminus. Note that this
        --   includes two possibilities: no route or exists, or every route
        --   which does exist has been cut (some member of the route dislodged).
        --
        resolveConvoy
            :: OrderObject Typical Convoy
            -> Maybe (FailureReason Typical Convoy)
        resolveConvoy convoyObject =
                convoyVoid convoyObject
            <|> convoyNoRoute convoyObject

        convoyVoid
            :: OrderObject Typical Convoy
            -> Maybe (FailureReason Typical Convoy)
        convoyVoid (ConvoyObject convoyingSubject convoyingTo) =
            case M.lookup convoyingFrom (dropAssumptionTags resolution) of
                Nothing -> Just ConvoyVoid
                Just (aunit, SomeResolved (MoveObject movingTo, _)) ->
                    if    convoyingUnit == alignedThing aunit
                       && convoyingTo == movingTo
                    then Nothing
                    else Just ConvoyVoid

          where

            convoyingFrom :: Zone
            convoyingFrom = Zone (snd convoyingSubject)

            convoyingUnit :: Unit
            convoyingUnit = fst convoyingSubject

        -- Route cut in case every convoy route which this convoy order
        -- participates in has at laest one of its convoyers dislodged.
        convoyNoRoute
            :: OrderObject Typical Convoy
            -> Maybe (FailureReason Typical Convoy)
        convoyNoRoute (ConvoyObject convoyingSubject convoyingTo) =
            case routesParticipatedIn of
                [] -> Just ConvoyNoRoute
                _ -> fmap ConvoyRouteCut cuttingSet

          where

            routes :: [[(Zone, Maybe (Aligned Subject))]]
            routes = rawConvoyRoutes resolution convoyingSubject convoyingTo

            routesParticipatedIn :: [[(Zone, Maybe (Aligned Subject))]]
            routesParticipatedIn = filter participates routes
              where
                participates = any (\(z, _) -> z == zone)

            cuttingSet :: Maybe [(Zone, Aligned Subject)]
            cuttingSet | length cutRoutes == length routesParticipatedIn = Just (nub (concat cutRoutes))
                       | otherwise = Nothing

            cutRoutes :: [[(Zone, Aligned Subject)]]
            cutRoutes = filter (not . null) (fmap cutRoute routesParticipatedIn)

            cutRoute
                :: [(Zone, Maybe (Aligned Subject))]
                -> [(Zone, Aligned Subject)]
            cutRoute = mapMaybe pickCutRoute

            pickCutRoute
                :: (Zone, Maybe (Aligned Subject))
                -> Maybe (Zone, Aligned Subject)
            pickCutRoute (z, m) = fmap ((,) z) m

    in  thisResolution

-- | Changes to a board as the result of a typical phase.
--   Nothing means no change, Just means the unit belonging to the great power
--   now lies at this Zone, and used to lie at the given ProvinceTarget.
change :: TypicalResolution -> Zone -> Maybe (Aligned Subject)
change res zone = M.foldWithKey folder Nothing res
  where
    folder
        :: Zone
        -> (Aligned Unit, SomeResolved OrderObject Typical)
        -> Maybe (Aligned Subject)
        -> Maybe (Aligned Subject)
    folder zone' (aunit, SomeResolved (object, resolution)) b = case object of
        MoveObject movingTo ->
            if Zone movingTo /= zone
            then b
            else case resolution of
                     Nothing -> let power = alignedGreatPower aunit
                                    unit = alignedThing aunit
                                    subj = align (unit, zoneProvinceTarget zone') power
                                in  Just subj
                     _ -> b
        _ -> b

-- Will want this:
--   TypicalResolution -> (Occupation, Dislodgement)

test1 :: M.Map Zone (Aligned Unit, SomeOrderObject Typical)
test1 = M.fromList [
      (Zone (Normal Brest), (align Army France, SomeOrderObject (MoveObject (Normal Paris))))
    , (Zone (Normal Burgundy), (align Army Italy, SomeOrderObject (MoveObject (Normal Paris))))

    ]

test2 :: M.Map Zone (Aligned Unit, SomeOrderObject Typical)
test2 = M.fromList [
      (Zone (Normal Brest), (align Army France, SomeOrderObject (MoveObject (Normal Paris))))
    , (Zone (Normal Burgundy), (align Army Italy, SomeOrderObject (MoveObject (Normal Paris))))
    , (Zone (Normal Paris), (align Army England, SomeOrderObject (MoveObject (Normal Paris))))

    ]

-- 2 moves into Paris bounce, while move out of Paris succeeds.
test3 :: M.Map Zone (Aligned Unit, SomeOrderObject Typical)
test3 = M.fromList [
      (Zone (Normal Brest), (align Army France, SomeOrderObject (MoveObject (Normal Paris))))
    , (Zone (Normal Burgundy), (align Army Italy, SomeOrderObject (MoveObject (Normal Paris))))
    , (Zone (Normal Paris), (align Army England, SomeOrderObject (MoveObject (Normal Picardy))))

    ]

-- 3-cycle of moves; all succeed.
test4 :: M.Map Zone (Aligned Unit, SomeOrderObject Typical)
test4 = M.fromList [
      (Zone (Normal Brest), (align Army France, SomeOrderObject (MoveObject (Normal Paris))))
    , (Zone (Normal Paris), (align Army Italy, SomeOrderObject (MoveObject (Normal Gascony))))
    , (Zone (Normal Gascony), (align Army England, SomeOrderObject (MoveObject (Normal Brest))))

    ]

-- 3-cycle of moves, interrupted by a bounce; all moves fail.
test5 :: M.Map Zone (Aligned Unit, SomeOrderObject Typical)
test5 = M.fromList [
      (Zone (Normal Brest), (align Army France, SomeOrderObject (MoveObject (Normal Paris))))
    , (Zone (Normal Paris), (align Army Italy, SomeOrderObject (MoveObject (Normal Gascony))))
    , (Zone (Normal Gascony), (align Army England, SomeOrderObject (MoveObject (Normal Brest))))

    , (Zone (Normal Picardy), (align Army England, SomeOrderObject (MoveObject (Normal Paris))))
    ]

-- A convoyed move; French orders succeed, English move bounces.
test6 :: M.Map Zone (Aligned Unit, SomeOrderObject Typical)
test6 = M.fromList [
      (Zone (Normal Brest), (align Army France, SomeOrderObject (MoveObject (Normal London))))
    , (Zone (Normal EnglishChannel), (align Fleet France, SomeOrderObject (ConvoyObject (Army, Normal Brest) (Normal London))))
    , (Zone (Normal Wales), (align Fleet England, SomeOrderObject (MoveObject (Normal EnglishChannel))))
    ]

-- Like test 6, except the convoy is dislodged and so the move fails.
test7 :: M.Map Zone (Aligned Unit, SomeOrderObject Typical)
test7 = M.fromList [
      (Zone (Normal Brest), (align Army France, SomeOrderObject (MoveObject (Normal London))))
    , (Zone (Normal EnglishChannel), (align Fleet France, SomeOrderObject (ConvoyObject (Army, Normal Brest) (Normal London))))
    , (Zone (Normal Wales), (align Fleet England, SomeOrderObject (MoveObject (Normal EnglishChannel))))
    , (Zone (Normal IrishSea), (align Fleet England, SomeOrderObject (SupportObject (Fleet, Normal Wales) (Normal EnglishChannel))))
    ]

-- Support is not cut when the move has no convoy.
test8 :: M.Map Zone (Aligned Unit, SomeOrderObject Typical)
test8 = M.fromList [
      (Zone (Normal Brest), (align Army France, SomeOrderObject (MoveObject (Normal London))))
    , (Zone (Normal EnglishChannel), (align Fleet France, SomeOrderObject (MoveObject (Normal EnglishChannel))))
    , (Zone (Normal Wales), (align Fleet England, SomeOrderObject (MoveObject (Normal EnglishChannel))))
    , (Zone (Normal London), (align Fleet England, SomeOrderObject (SupportObject (Fleet, Normal Wales) (Normal EnglishChannel))))
    ]

-- Simple convoy paradox!!!
test9 :: M.Map Zone (Aligned Unit, SomeOrderObject Typical)
test9 = M.fromList [
      (Zone (Normal Brest), (align Army France, SomeOrderObject (MoveObject (Normal London))))
    , (Zone (Normal EnglishChannel), (align Fleet France, SomeOrderObject (ConvoyObject (Army, Normal Brest) (Normal London))))
    , (Zone (Normal Wales), (align Fleet England, SomeOrderObject (MoveObject (Normal EnglishChannel))))
    , (Zone (Normal London), (align Fleet England, SomeOrderObject (SupportObject (Fleet, Normal Wales) (Normal EnglishChannel))))
    ]

-- Pandin's paradox!!!
--
-- England:
-- F Wal - ENG
-- F Lon S F Wal - ENG
--
-- France:
-- A Bre - ENG - Lon
-- F ENG C A Bre - Lon
-- A Yor S A Bre - Lon
--
-- Germany:
-- F Bel - ENG
-- F NTH S F Bel - ENG 
--
-- Germany and England standoff in ENG, the French convoying fleet succeeds, yet
-- the move fails. This is to be expected; a convoy CAN succeed without the
-- required move succeeding, so long as the move was issued.
test10 :: M.Map Zone (Aligned Unit, SomeOrderObject Typical)
test10 = M.fromList [

      (Zone (Normal Brest), (align Army France, SomeOrderObject (MoveObject (Normal London))))
    , (Zone (Normal EnglishChannel), (align Fleet France, SomeOrderObject (ConvoyObject (Army, Normal Brest) (Normal London))))
    , (Zone (Normal Yorkshire), (align Army France, SomeOrderObject (SupportObject (Army, Normal Brest) (Normal London))))

    , (Zone (Normal Wales), (align Fleet England, SomeOrderObject (MoveObject (Normal EnglishChannel))))
    , (Zone (Normal London), (align Fleet England, SomeOrderObject (SupportObject (Fleet, Normal Wales) (Normal EnglishChannel))))

    , (Zone (Normal Belgium), (align Fleet Germany, SomeOrderObject (MoveObject (Normal EnglishChannel))))
    , (Zone (Normal NorthSea), (align Fleet Germany, SomeOrderObject (SupportObject (Fleet, Normal Belgium) (Normal EnglishChannel))))
    ]

{-
-- | Retreat phase resolution groups all withdraws by target, and fails elements
--   of groups with size > 1.
--   All surrenders of course succeed.
resolveRetreat :: PhaseResolution Retreat
resolveRetreat zonedOrders = M.mapWithKey resolve zonedOrders
  where
    resolve
        :: Zone
        -> (Aligned Unit, SomeOrderObject Retreat)
        -> (Aligned Unit, SomeResolved OrderObject Retreat)
    resolve zone (aunit, SomeOrderObject object) = case object of
        SurrenderObject -> (aunit, SomeResolved (object, Nothing))
        WithdrawObject _ -> (aunit, SomeResolved (object, resolution))
          where
            resolution :: Maybe (FailureReason Retreat Withdraw)
            resolution = case filter (/= thisOrder) (withdrawsToZone (Zone (withdrawTarget object)) zonedOrders) of
                [] -> Nothing
                xs -> Just (WithdrawCollision xs)
            thisOrder :: Order Retreat Withdraw
            thisOrder = makeOrder zone (aunit, object)

withdrawsToZone
    :: Zone
    -> M.Map Zone (Aligned Unit, SomeOrderObject Retreat)
    -> [Order Retreat Withdraw]
withdrawsToZone zone = M.elems . M.mapWithKey makeOrder . M.mapMaybe (pickWithdrawAt zone)
  where
    pickWithdrawAt
        :: Zone
        -> (Aligned Unit, SomeOrderObject Retreat)
        -> Maybe (Aligned Unit, OrderObject Retreat Withdraw)
    pickWithdrawAt zone (aunit, SomeOrderObject retreatObject) = case retreatObject of
        SurrenderObject -> Nothing
        WithdrawObject pt ->
            if Zone pt == zone
            then Just (aunit, retreatObject)
            else Nothing

-- | Adjust phase is resolved very easily, as self-consistent disband and
--   build orders always succeed.
resolveAdjust :: PhaseResolution Adjust
resolveAdjust = M.map (\(aunit, SomeOrderObject object) -> (aunit, SomeResolved (object, Nothing)))
-}

type Resolved (k :: Phase -> OrderType -> *) (phase :: Phase) (order :: OrderType) =
    (k phase order, Maybe (FailureReason phase order))

data SomeResolved (k :: Phase -> OrderType -> *) phase where
    SomeResolved :: Resolved k phase order -> SomeResolved k phase

deriving instance Show (SomeResolved OrderObject phase)
deriving instance Show (SomeResolved Order phase)

instance Eq (SomeResolved OrderObject phase) where
    SomeResolved (object1, res1) == SomeResolved (object2, res2) =
           object1 `orderObjectEqual` object2
        && case (res1, res2) of
               (Just r1, Just r2) -> failureReasonEqual r1 r2
               (Nothing, Nothing) -> True
               _ -> False

withSomeResolved
  :: (forall order . Resolved k phase order -> t) -> SomeResolved k phase -> t
withSomeResolved f term = case term of
    SomeResolved x -> f x

someResolvedValidOrder :: SomeResolved Order phase -> Valid (SomeOrder phase)
someResolvedValidOrder res = case res of
    SomeResolved (order, _) -> Valid (SomeOrder order)

-- | Enumeration of reasons why an order could not succeed.
data FailureReason (phase :: Phase) (order :: OrderType) where

    MoveOverpowered :: AtLeast One (Aligned Subject) -> FailureReason Typical Move

    MoveBounced :: AtLeast One (Aligned Subject) -> FailureReason Typical Move

    -- | The move would dislodge the player's own unit.
    MoveFriendlyDislodge :: Unit -> FailureReason Typical Move

    MoveNoConvoy :: FailureReason Typical Move

    MoveConvoyParadox :: FailureReason Typical Move

    -- | The supported unit did not give an order consistent with the support
    --   order.
    SupportVoid :: FailureReason Typical Support

    -- | The supporting unit was attacked from a province other than the one
    --   into which the support was directed.
    SupportCut :: AtLeast One (Aligned Subject) -> FailureReason Typical Support

    -- | The supporting unit was overpowered by a move from the province into
    --   which the support was directed.
    SupportDislodged :: Aligned Subject -> FailureReason Typical Support

    ConvoyVoid :: FailureReason Typical Convoy

    ConvoyNoRoute :: FailureReason Typical Convoy

    ConvoyRouteCut :: [(Zone, Aligned Subject)] ->  FailureReason Typical Convoy

    -- | The unit withdraws into the same province as some other unit(s).
    WithdrawCollision :: [Order Retreat Withdraw] -> FailureReason Retreat Withdraw

    -- Surrender orders and adjust phase orders can never fail; if they're
    -- valid, they succeed!

deriving instance Show (FailureReason phase order)
deriving instance Eq (FailureReason phase order)

failureReasonEqual
    :: FailureReason phase order
    -> FailureReason phase' order'
    -> Bool
failureReasonEqual r1 r2 = case (r1, r2) of
    (MoveOverpowered x, MoveOverpowered y) -> x == y
    (MoveBounced x, MoveBounced y) -> x == y
    (MoveFriendlyDislodge x, MoveFriendlyDislodge y) -> x == y
    (MoveNoConvoy, MoveNoConvoy) -> True
    (MoveConvoyParadox, MoveConvoyParadox) -> True
    (SupportVoid, SupportVoid) -> True
    (SupportCut x, SupportCut y) -> x == y
    (SupportDislodged x, SupportDislodged y) -> x == y
    (ConvoyVoid, ConvoyVoid) -> True
    (ConvoyNoRoute, ConvoyNoRoute) -> True
    (ConvoyRouteCut x, ConvoyRouteCut y) -> x == y
    (WithdrawCollision x, WithdrawCollision y) -> x == y
    _ -> False
