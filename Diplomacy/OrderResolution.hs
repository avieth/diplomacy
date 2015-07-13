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
import Data.Maybe
import Data.AtLeast
import Data.TypeNat.Nat
import Data.TypeNat.Vect
import Data.Functor.Identity
import Data.Traversable (sequenceA)
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

-- | Enumeration of reasons why order resolution could not proceed.
data ResolutionError (phase :: Phase) where
    InvalidOrder :: ResolutionError phase
--    InvalidOrder :: Invalid Order phase order -> ResolutionError phase

type TypicalResolution
    = M.Map Zone (Aligned Unit, SomeResolved OrderObject Typical)

typicalResolution
    :: M.Map Zone (Aligned Unit, SomeOrderObject Typical)
    -> TypicalResolution
typicalResolution input =
    let res = M.mapWithKey (resolveSomeOrderTypical res) input
    in  res

resolveSomeOrderTypical
    :: TypicalResolution
    -> Zone
    -> (Aligned Unit, SomeOrderObject Typical)
    -> (Aligned Unit, SomeResolved OrderObject Typical)
resolveSomeOrderTypical res zone (aunit, SomeOrderObject object) =

    let resolution :: SomeResolved OrderObject Typical
        resolution = case object of
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
        --   Move2Cycle : the move forms a 2-cycle with some other move and
        --   both have no successful convoy routes.
        --   MoveOverpowered : there is some other move of strictly greater
        --   support into this move's target.
        --   MoveBounced : this move is not a hold, and it is not a dominator
        --   at its target.
        --   MoveFriendlyDislodge : the move would dislodge a friendly unit.
        resolveMove
            :: OrderObject Typical Move
            -> Maybe (FailureReason Typical Move)
        resolveMove moveObject =
                moveNoConvoy zone moveObject
            <|> moveConvoyParadox zone moveObject
            <|> move2Cycle zone moveObject
            <|> moveOverpowered moveObject
            <|> moveBounced moveObject
            <|> moveSelfDislodge moveObject

        isHold :: OrderObject Typical Move -> Bool
        isHold (MoveObject movingTo) = Zone movingTo == zone

        -- Number of successful non-void supports of a Subject into a given
        -- Zone.
        calculateSupport :: Zone -> Subject -> Int
        calculateSupport zone subject = M.fold folder 0 res
          where
            folder
              :: (Aligned Unit, SomeResolved OrderObject Typical)
              -> Int
              -> Int
            folder (_, SomeResolved (object, resolution)) b = case object of
                SupportObject supportSubject supportTo ->
                    if    supportSubject /= subject
                       || Zone supportTo /= zone
                    then b
                    else case resolution of
                             Nothing -> b + 1
                             _ -> b
                _ -> b

        -- Must incorporate the move OUT of here.
        -- If it does not or exist or it succeeds, no change.
        -- If it exists and fails, then
        --     if it's a friendly unit, always fail with self dislodge.
        --     if it's not friendly, fail iff this move has <= 1 support.
        --
        -- To do this, we'll need to be able to get the resolutions assuming
        -- this move was not issued.
        retract
            :: TypicalResolution
            -> M.Map Zone (Aligned Unit, SomeOrderObject Typical)
        retract =
            M.map
              (\(aunit, SomeResolved (object, _)) ->
                (aunit, SomeOrderObject object)
              )

        resWithoutThis :: TypicalResolution
        resWithoutThis = typicalResolution . (M.delete zone) . retract $ res

        -- If Just asubj then there was either
        --   1. a failed move from the target of the input move
        --   2. a support or convoy at the target of the input move
        --   3. a hold (whether successful or unsucessful) at the target of the
        --      input move
        -- where the aligned subject determines the issuing power,
        -- the unit which would have moved, and the place to which it would have
        -- moved.
        failedExodus
            :: OrderObject Typical Move
            -> Maybe (Aligned Subject)
        failedExodus (MoveObject movingTo) = case M.lookup (Zone movingTo) resWithoutThis of
            Just (aunit, SomeResolved (object, Just _)) -> Just $
                align (alignedThing aunit, movingTo') (alignedGreatPower aunit)
              where
                movingTo' = case object of
                    MoveObject pt -> pt
                    _ -> movingTo
            Just (aunit, SomeResolved (MoveObject movingTo', Nothing)) ->
                if Zone movingTo == Zone movingTo'
                then Just $ align (alignedThing aunit, movingTo') (alignedGreatPower aunit)
                else Nothing
            _ -> Nothing

        competingOrders :: OrderObject Typical Move -> [Aligned Subject]
        competingOrders (MoveObject movingTo) = M.foldWithKey folder [] res
          where
            folder
                :: Zone
                -> (Aligned Unit, SomeResolved OrderObject Typical)
                -> [Aligned Subject]
                -> [Aligned Subject]
            folder zone' (aunit, SomeResolved (object, _)) b =
                if    zone' == zone  -- Must rule out including this move.
                   || destination /= Zone (movingTo)
                   || isConvoyMoveWithNoConvoyRoute zone' object
                then b
                else (align (alignedThing aunit, zoneProvinceTarget zone') (alignedGreatPower aunit)) : b
              where
                destination = case object of
                    MoveObject pt -> Zone pt
                    _ -> zone'

        isConvoyMoveWithNoConvoyRoute :: Zone -> OrderObject Typical order -> Bool
        isConvoyMoveWithNoConvoyRoute zone object = case object of
            -- No good; these functions are local and thus not quite
            -- so reusable (zone is captured).
            -- Could free it up... or maybe put it into the resolution
            -- map?
            MoveObject _ -> isJust (moveNoConvoy zone object <|> moveConvoyParadox zone object)
            _ -> False

        supportedCompetingOrders :: OrderObject Typical Move -> [(Aligned Subject, Int)]
        supportedCompetingOrders moveObject =
            let zone = Zone (moveTarget moveObject)
                xs = competingOrders moveObject
                ys = fmap (calculateSupport zone . alignedThing) xs
            in xs `zip` ys

        sortedSupportedCompetingOrders :: OrderObject Typical Move -> [(Aligned Subject, Int)]
        sortedSupportedCompetingOrders =
            sortBy comparator . supportedCompetingOrders
          where
            comparator x y = compare (Down . snd $ x) (Down . snd $ y)

        localSupport :: OrderObject Typical Move -> Int
        localSupport moveObject =
            let zone' = Zone (moveTarget moveObject)
                subj = (alignedThing aunit, zoneProvinceTarget zone)
            in  calculateSupport zone' subj

        moveOverpowered
            :: OrderObject Typical Move
            -> Maybe (FailureReason Typical Move)
        moveOverpowered moveObject = case sortedSupportedCompetingOrders moveObject of
            [] -> Nothing
            (x : xs) ->
                if    snd x > localSupport moveObject
                   && alignedGreatPower (fst x) /= alignedGreatPower aunit
                then Just (MoveOverpowered (AtLeast (VCons (fst x) VNil) (fmap fst xs)))
                else Nothing

        moveBounced
            :: OrderObject Typical Move
            -> Maybe (FailureReason Typical Move)
        moveBounced moveObject = case sortedSupportedCompetingOrders moveObject of
            [] -> case failedExodus moveObject of
                      Just alignedSubj ->
                          if localSupport moveObject == 0
                          then Just (MoveBounced (AtLeast (VCons alignedSubj VNil) []))
                          else Nothing
                      Nothing -> Nothing
            -- Note in this case, failedExodus doesn't matter, since the local
            -- support is at least 0. If it doesn't coincide with that of x,
            -- then it must be 1 at least, meaning it overcomes the incumbant.
            (x : xs) ->
                if    snd x == localSupport moveObject
                   && not (isHold moveObject)
                then Just (MoveBounced (AtLeast (VCons (fst x) VNil) (fmap fst (filter ((==) (localSupport moveObject) . snd) xs))))
                else Nothing

        moveSelfDislodge
            :: OrderObject Typical Move
            -> Maybe (FailureReason Typical Move)
        moveSelfDislodge moveObject = case failedExodus moveObject of
            Just alignedSubj ->
                if   alignedGreatPower alignedSubj == alignedGreatPower aunit
                then Just (MoveSelfDislodge (subjectUnit (alignedThing alignedSubj)))
                else Nothing
            _ -> Nothing

        paradoxInducingSupport
            :: OrderObject Typical Move
            -> Maybe (OrderObject Typical Support)
        paradoxInducingSupport (MoveObject movingTo) = case M.lookup (Zone movingTo) res of
            Just (aunit, SomeResolved (s@(SupportObject _ _), _)) -> Just s
            _ -> Nothing

        paradoxInducingConvoyZone
            :: OrderObject Typical Move
            -> Maybe Zone
        paradoxInducingConvoyZone = fmap (Zone . supportTarget) . paradoxInducingSupport

        -- Any move between non-adjacent provinces is deemed to require a
        -- convoy, even if both provinces are inland. Order validation rules
        -- out those cases though.
        moveRequiresConvoy :: Zone -> OrderObject Typical Move -> Bool
        moveRequiresConvoy zone (MoveObject movingTo) =
            not (isSameOrAdjacent (ptProvince movingTo) (ptProvince (zoneProvinceTarget zone)))

        moveConvoyRoutes
            :: Zone
            -> OrderObject Typical Move
            -> [[(Zone, Maybe (Aligned Subject))]]
        moveConvoyRoutes zone (MoveObject movingTo) =
            convoyRoutes (alignedThing aunit, zoneProvinceTarget zone) movingTo res

        isParadoxRoute
            :: OrderObject Typical Move
            -> [(Zone, Maybe (Aligned Subject))]
            -> Bool
        isParadoxRoute moveObject =
            any (\(z, _) -> Just z == paradoxInducingConvoyZone moveObject)

        moveParadoxConvoyRoutes zone moveObject =
            filter (isParadoxRoute moveObject) (moveConvoyRoutes zone moveObject)

        moveNonParadoxConvoyRoutes zone moveObject =
            filter (not . (isParadoxRoute moveObject)) (moveConvoyRoutes zone moveObject)

        moveSuccessfulConvoyRoutes
            :: Zone
            -> OrderObject Typical Move
            -> [[(Zone, Maybe (Aligned Subject))]]
        moveSuccessfulConvoyRoutes zone =
            filter isSuccessful . moveNonParadoxConvoyRoutes zone
          where
            isSuccessful = all (isNothing . snd)

        moveNoConvoy
            :: Zone
            -> OrderObject Typical Move
            -> Maybe (FailureReason Typical Move)
        moveNoConvoy zone moveObject =
            if    moveRequiresConvoy zone moveObject
               && null (moveSuccessfulConvoyRoutes zone moveObject)
               && null (moveParadoxConvoyRoutes zone moveObject)
            then Just MoveNoConvoy
            else Nothing

        moveConvoyParadox
            :: Zone
            -> OrderObject Typical Move
            -> Maybe (FailureReason Typical Move)
        moveConvoyParadox zone moveObject =
            if    moveRequiresConvoy zone moveObject
               && null (moveSuccessfulConvoyRoutes zone moveObject)
               && not (null (moveParadoxConvoyRoutes zone moveObject))
            then Just MoveConvoyParadox
            else Nothing

        move2Cycle
            :: Zone
            -> OrderObject Typical Move
            -> Maybe (FailureReason Typical Move)
        move2Cycle zone moveObject@(MoveObject movingTo) =
            if isHold moveObject
            then Nothing
            else case M.lookup (Zone movingTo) res of
                Just (aunit, SomeResolved (moveObject'@(MoveObject movingTo'), _)) ->
                    if Zone movingTo' /= zone
                    then Nothing
                    else if    not (null (moveSuccessfulConvoyRoutes (Zone movingTo) moveObject'))
                            || not (null (moveSuccessfulConvoyRoutes (Zone movingTo') moveObject))
                         then Nothing
                         else Just (Move2Cycle aunit)
                _ -> Nothing


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
            case M.lookup supportingFrom res of
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
            offendingMoves = M.elems (M.mapMaybeWithKey pickOffendingMove res)

            pickOffendingMove
                :: Zone
                -> (Aligned Unit, SomeResolved OrderObject Typical)
                -> Maybe (Aligned Subject)
            pickOffendingMove zone (aunit', SomeResolved (object, _)) =
                case object of
                    MoveObject movingTo ->
                        if    Zone movingTo == supportingFrom
                           && Zone supportingTo /= zone
                           && not (isConvoyMoveWithNoConvoyRoute zone object)
                        then Just $ align (alignedThing aunit', zoneProvinceTarget zone) (alignedGreatPower aunit')
                        else Nothing
                    _ -> Nothing

        supportDislodged
            :: OrderObject Typical Support
            -> Maybe (FailureReason Typical Support)
        supportDislodged _ = case change res zone of
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
            case M.lookup convoyingFrom res of
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

        convoyNoRoute
            :: OrderObject Typical Convoy
            -> Maybe (FailureReason Typical Convoy)
        convoyNoRoute (ConvoyObject convoyingSubject convoyingTo) =
            case routes of
                [] -> Just ConvoyNoRoute
                _ -> fmap ConvoyRouteCut cuttingSet

          where

            routes :: [[(Zone, Maybe (Aligned Subject))]]
            routes = convoyRoutes convoyingSubject convoyingTo res

            cuttingSet :: Maybe [(Zone, Aligned Subject)]
            cuttingSet | length cutRoutes == length routes = Just (nub (concat cutRoutes))
                       | otherwise = Nothing

            cutRoutes :: [[(Zone, Aligned Subject)]]
            cutRoutes = filter (not . null) (fmap cutRoute routes)

            cutRoute
                :: [(Zone, Maybe (Aligned Subject))]
                -> [(Zone, Aligned Subject)]
            cutRoute = mapMaybe pickCutRoute

            pickCutRoute
                :: (Zone, Maybe (Aligned Subject))
                -> Maybe (Zone, Aligned Subject)
            pickCutRoute (z, m) = fmap ((,) z) m

    in  (aunit, resolution)

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


-- Just for testing of convoyRoutes.
convoys :: TypicalResolution
convoys = M.fromList [
    --  (Zone (Normal NorthSea), (align Fleet England, firstConvoy))
    --, (Zone (Normal HelgolandBright), (align Fleet England, firstConvoy))
    --, (Zone (Normal NorwegianSea), (align Fleet England, secondConvoy))
    --, (Zone (Normal BarentsSea), (align Fleet England, thirdConvoy))
      (Zone (Normal EnglishChannel), (align Fleet England, SomeResolved (ConvoyObject (Army, Normal London) (Normal Portugal), undefined)))
    , (Zone (Normal MidAtlanticOcean), (align Fleet England, SomeResolved (ConvoyObject (Army, Normal London) (Normal Portugal), undefined)))
    , (Zone (Normal London), (align Army France, SomeResolved (MoveObject (Normal Portugal), undefined)))
    , (Zone (Normal Spain), (align Fleet France, SomeResolved (SupportObject (Army, Normal London) (Normal Portugal), undefined)))
    , (Zone (Normal Portugal), (align Army Italy, SomeResolved (MoveObject (Normal Spain), undefined)))
    , (Zone (Normal Spain), (align Army Italy, SomeResolved (MoveObject (Normal Portugal), undefined)))
    ]
  where
    firstConvoy = SomeResolved (ConvoyObject (Army, Normal Holland) (Normal StPetersburg), undefined)
    secondConvoy = firstConvoy
    thirdConvoy = firstConvoy

convoys' = fmap (\(x, SomeResolved (order, _)) -> (x, SomeOrderObject order)) convoys

-- | All convoy routes which connect the subject to the given ProvinceTarget.
--   Each element of a route gives its zone (zone of the convoying fleet which
--   composese the route) as well as an indication of whether it was
--   dislodged.
convoyRoutes
    :: Subject
    -> ProvinceTarget
    -> TypicalResolution
    -> [[(Zone, Maybe (Aligned Subject))]]
convoyRoutes (unit, ptFrom) ptTo res = do
    let pool = viableConvoyZones
    let starters = filter isStarter pool
    s <- starters
    let routes = paths pool ptTo s
    (fmap . fmap) tagWithChange routes

  where

    tagWithChange :: Zone -> (Zone, Maybe (Aligned Subject))
    tagWithChange zone = (zone, change res zone)

    viableConvoyZones :: [Zone]
    viableConvoyZones = M.keys (M.filter isViableConvoy res)

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

    -- | The move is part of an unconvoyed 2-cycle of moves.
    Move2Cycle :: Aligned Unit -> FailureReason Typical Move

    -- | The move would dislodge the player's own unit.
    MoveSelfDislodge :: Unit -> FailureReason Typical Move

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
    (Move2Cycle x, Move2Cycle y) -> x == y
    (MoveSelfDislodge x, MoveSelfDislodge y) -> x == y
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
