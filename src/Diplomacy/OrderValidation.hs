{-|
Module      : Diplomacy.OrderValidation
Description : Definition of order validation
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Diplomacy.OrderValidation (

    ValidityCharacterization(..)
  , ArgumentList(..)

  , ValidityCriterion(..)
  , SomeValidityCriterion(..)
  , AdjustSetValidityCriterion(..)
  , ValidityTag
  , AdjustSetValidityTag

  , synthesize
  , analyze

  , moveVOC
  , supportVOC
  , convoyVOC
  , surrenderVOC
  , withdrawVOC

  , AdjustSubjects(..)
  , disbandSubjectVOC
  , buildSubjectVOC
  , continueSubjectVOC
  , adjustSubjectsVOC

  ) where

import GHC.Exts (Constraint)
import Control.Monad
import Control.Applicative
import qualified Data.Map as M
import qualified Data.Set as S
import Data.MapUtil
import Data.AtLeast
import Data.Functor.Identity
import Data.Functor.Constant
import Data.Functor.Compose
import Data.List as L
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
import Diplomacy.ZonedSubject
import Diplomacy.Occupation
import Diplomacy.Dislodgement
import Diplomacy.Control
import Diplomacy.SupplyCentreDeficit
import Diplomacy.OrderResolution

import Debug.Trace

-- Each one of these constructors is associated with a set.
data ValidityCriterion (phase :: Phase) (order :: OrderType) where

    MoveValidSubject :: ValidityCriterion Typical Move
    MoveUnitCanOccupy :: ValidityCriterion Typical Move
    MoveReachable :: ValidityCriterion Typical Move

    SupportValidSubject :: ValidityCriterion Typical Support
    SupporterAdjacent :: ValidityCriterion Typical Support
    SupporterCanOccupy :: ValidityCriterion Typical Support
    SupportedCanDoMove :: ValidityCriterion Typical Support

    ConvoyValidSubject :: ValidityCriterion Typical Convoy
    ConvoyValidConvoySubject :: ValidityCriterion Typical Convoy
    ConvoyValidConvoyTarget :: ValidityCriterion Typical Convoy

    SurrenderValidSubject :: ValidityCriterion Retreat Surrender

    WithdrawValidSubject :: ValidityCriterion Retreat Withdraw
    WithdrawAdjacent :: ValidityCriterion Retreat Withdraw
    WithdrawUnoccupiedZone :: ValidityCriterion Retreat Withdraw
    WithdrawUncontestedZone :: ValidityCriterion Retreat Withdraw
    WithdrawNotDislodgingZone :: ValidityCriterion Retreat Withdraw

    ContinueValidSubject :: ValidityCriterion Adjust Continue
    DisbandValidSubject :: ValidityCriterion Adjust Disband
    BuildValidSubject :: ValidityCriterion Adjust Build

deriving instance Show (ValidityCriterion phase order)
deriving instance Eq (ValidityCriterion phase order)
deriving instance Ord (ValidityCriterion phase order)

data SomeValidityCriterion (phase :: Phase) where
    SomeValidityCriterion :: ValidityCriterion phase order -> SomeValidityCriterion phase

instance Show (SomeValidityCriterion phase) where
    show (SomeValidityCriterion vc) = case vc of
        MoveValidSubject -> show vc
        MoveUnitCanOccupy -> show vc
        MoveReachable -> show vc
        SupportValidSubject -> show vc
        SupporterAdjacent -> show vc
        SupporterCanOccupy -> show vc
        SupportedCanDoMove -> show vc
        ConvoyValidSubject -> show vc
        ConvoyValidConvoySubject -> show vc
        ConvoyValidConvoyTarget -> show vc
        SurrenderValidSubject -> show vc
        WithdrawValidSubject -> show vc
        WithdrawAdjacent -> show vc
        WithdrawUnoccupiedZone -> show vc
        WithdrawUncontestedZone -> show vc
        WithdrawNotDislodgingZone -> show vc
        ContinueValidSubject -> show vc
        DisbandValidSubject -> show vc
        BuildValidSubject -> show vc

instance Eq (SomeValidityCriterion phase) where
    SomeValidityCriterion vc1 == SomeValidityCriterion vc2 = case (vc1, vc2) of
        (MoveValidSubject, MoveValidSubject) -> True
        (MoveUnitCanOccupy, MoveUnitCanOccupy) -> True
        (MoveReachable, MoveReachable) -> True
        (SupportValidSubject, SupportValidSubject) -> True
        (SupporterAdjacent, SupporterAdjacent) -> True
        (SupporterCanOccupy, SupporterCanOccupy) -> True
        (SupportedCanDoMove, SupportedCanDoMove) -> True
        (ConvoyValidSubject, ConvoyValidSubject) -> True
        (ConvoyValidConvoySubject, ConvoyValidConvoySubject) -> True
        (ConvoyValidConvoyTarget, ConvoyValidConvoyTarget) -> True
        (SurrenderValidSubject, SurrenderValidSubject) -> True
        (WithdrawValidSubject, WithdrawValidSubject) -> True
        (WithdrawAdjacent, WithdrawAdjacent) -> True
        (WithdrawUnoccupiedZone, WithdrawUnoccupiedZone) -> True
        (WithdrawUncontestedZone, WithdrawUncontestedZone) -> True
        (WithdrawNotDislodgingZone, WithdrawNotDislodgingZone) -> True
        (ContinueValidSubject, ContinueValidSubject) -> True
        (DisbandValidSubject, DisbandValidSubject) -> True
        (BuildValidSubject, BuildValidSubject) -> True
        _ -> False

instance Ord (SomeValidityCriterion phase) where
    SomeValidityCriterion vc1 `compare` SomeValidityCriterion vc2 =
        show vc1 `compare` show vc2

data AdjustSetValidityCriterion where
    RequiredNumberOfDisbands :: AdjustSetValidityCriterion
    AdmissibleNumberOfBuilds :: AdjustSetValidityCriterion
    OnlyContinues :: AdjustSetValidityCriterion

deriving instance Eq AdjustSetValidityCriterion
deriving instance Ord AdjustSetValidityCriterion
deriving instance Show AdjustSetValidityCriterion

-- | All ProvinceTargets which a unit can legally occupy.
unitCanOccupy :: Unit -> S.Set ProvinceTarget
unitCanOccupy unit = case unit of
    Army -> S.map Normal . S.filter (not . isWater) $ S.fromList [minBound..maxBound]
    Fleet -> S.fromList $ do
        pr <- [minBound..maxBound]
        guard (not (isInland pr))
        case provinceCoasts pr of
            [] -> return $ Normal pr
            xs -> fmap Special xs

-- | All places to which a unit could possibly move (without regard for
--   occupation rules as specified by unitCanOccupy).
--   The Occupation parameter is needed to determine which convoys are possible.
--   If it's nothing, we don't consider convoy routes.
validMoveAdjacency :: Maybe Occupation -> Subject -> S.Set ProvinceTarget
validMoveAdjacency occupation subject = case subjectUnit subject of
    Army -> case occupation of
        Nothing -> S.fromList $ neighbours pt
        Just o -> (S.fromList $ neighbours pt) `S.union` (S.map Normal (convoyTargets o pr))
    Fleet -> S.fromList $ do
        n <- neighbours pt
        let np = ptProvince n
        let ppt = ptProvince pt
        -- If we have two coastal places, we must guarantee that they have a
        -- common coast.
        guard (not (isCoastal np) || not (isCoastal ppt) || not (null (commonCoasts pt n)))
        return n
  where
    pt = subjectProvinceTarget subject
    pr = ptProvince pt

convoyPaths :: Occupation -> Province -> [(Province, [Province])]
convoyPaths occupation pr =
    filter ((/=) pr . fst) . fmap (\(x, y, z) -> (x, y : z)) . paths occupiedByFleet pickCoastal . pure $ pr
  where
    occupiedByFleet pr = case provinceOccupier pr occupation of
        Just aunit -> alignedThing aunit == Fleet
        _ -> False
    pickCoastal pr = if isCoastal pr then Just pr else Nothing

convoyTargets :: Occupation -> Province -> S.Set Province
convoyTargets occupation = S.fromList . fmap fst . convoyPaths occupation

validMoveTargets
    :: Maybe Occupation
    -> Subject
    -> S.Set ProvinceTarget
validMoveTargets maybeOccupation subject =
    (validMoveAdjacency maybeOccupation subject)
    `S.intersection`
    (unitCanOccupy (subjectUnit subject))

-- | Valid support targets are any place where this subject could move without
--   a convoy (this excludes the subject's own province target), and such that
--   the common coast constraint is relaxed (a Fleet in Marseilles can support
--   into Spain NC for example).
validSupportTargets
    :: Subject
    -> S.Set ProvinceTarget
validSupportTargets subject = S.fromList $ do
    x <- S.toList $ validMoveAdjacency Nothing subject
    guard (S.member x (unitCanOccupy (subjectUnit subject)))
    provinceTargetCluster x

-- | Given two ProvinceTargets--the place from which support comes, and the
--   place to which support is directed--we can use an Occupation to discover
--   every subject which could be supported by this hypothetical supporter.
validSupportSubjects
    :: Occupation
    -> ProvinceTarget -- ^ Source
    -> ProvinceTarget -- ^ Target
    -> S.Set Subject
validSupportSubjects occupation source target = M.foldrWithKey f S.empty occupation
  where
    f zone aunit =
        if    Zone source /= zone
           -- validMoveTargets will give us non-hold targets, so we explicitly
           -- handle the case of a hold.
           && (Zone target == zone
           -- If the subject here could move to the target, then it's a valid
           -- support target. We are careful *not* to use Zone-equality here,
           -- because in the case of supporting fleets into coastal territories,
           -- we want to rule out supporting to an unreachable coast.
           || S.member target (validMoveTargets (Just occupation) subject'))
        then S.insert subject'
        else id
      where
        subject' = (alignedThing aunit, zoneProvinceTarget zone)

-- | Subjects which could act as convoyers: fleets in water.
validConvoyers
    :: Maybe GreatPower
    -> Occupation
    -> S.Set Subject
validConvoyers greatPower = M.foldrWithKey f S.empty
  where
    f zone aunit = case unit of
        Fleet -> if    isWater (ptProvince pt)
                    && (  greatPower == Nothing
                       || greatPower == Just (alignedGreatPower aunit)
                       )
                 then S.insert (unit, pt)
                 else id
        _ -> id
      where
        pt = zoneProvinceTarget zone
        unit = alignedThing aunit

-- | Subjects which could be convoyed: armies on coasts.
validConvoySubjects
    :: Occupation
    -> S.Set Subject
validConvoySubjects = M.foldrWithKey f S.empty
  where
    f zone aunit = if unit == Army && isCoastal (ptProvince pt)
                   then S.insert (unit, pt)
                   else id
      where
        unit = alignedThing aunit
        pt = zoneProvinceTarget zone

-- | Valid convoy destinations: those reachable by some path of fleets in
--   water which includes the convoyer subject, and initiates at the convoying
--   subject's province target.
validConvoyTargets
    :: Occupation
    -> Subject
    -> Subject
    -> S.Set ProvinceTarget
validConvoyTargets occupation subjectConvoyer subjectConvoyed =
    let allConvoyPaths = convoyPaths occupation prConvoyed
        convoyPathsWithThis = filter (elem prConvoyer . snd) allConvoyPaths
    in  S.fromList (fmap (Normal . fst) convoyPathsWithThis)
  where
    prConvoyer = ptProvince (subjectProvinceTarget subjectConvoyer)
    prConvoyed = ptProvince (subjectProvinceTarget subjectConvoyed)

-- Would be nice to have difference, to simulate "not". Then we could say
-- "not contested", "not attacking province" and "not occupied" and providing
-- those contested, attacking province, and occupied sets, rather than
-- providing their complements.
--
-- Ok, so for withdraw, we wish to say
--
--   subject : valid subject
--   target :   valid unconvoyed move target
--            & not contested area
--            & not dislodging province (of subject's province target)
--            & not occupied province
setOfAllProvinceTargets :: S.Set ProvinceTarget
setOfAllProvinceTargets = S.fromList [minBound..maxBound]

setOfAllZones :: S.Set Zone
setOfAllZones = S.map Zone setOfAllProvinceTargets

zoneSetToProvinceTargetSet :: S.Set Zone -> S.Set ProvinceTarget
zoneSetToProvinceTargetSet = S.fold f S.empty
  where
    f zone = S.union (S.fromList (provinceTargetCluster (zoneProvinceTarget zone)))

occupiedZones :: Occupation -> S.Set Zone
occupiedZones = S.map (Zone . snd) . S.fromList . allSubjects Nothing

-- A zone is contested iff there is at least one bounced move order to it, and
-- no successful move order to it.
contestedZones
    :: M.Map Zone (Aligned Unit, SomeResolved OrderObject Typical)
    -> S.Set Zone
contestedZones = M.foldrWithKey g S.empty . M.foldr f M.empty
  where

    f :: (Aligned Unit, SomeResolved OrderObject Typical)
      -> M.Map Zone Bool
      -> M.Map Zone Bool
    f (aunit, SomeResolved (object, res)) = case object of
        MoveObject pt -> case res of
            Just (MoveBounced _) -> M.alter alteration (Zone pt)
            _ -> id
          where
            alteration (Just bool) = case res of
                Nothing -> Just False
                _ -> Just bool
            alteration Nothing = case res of
                Nothing -> Just False
                _ -> Just True
        _ -> id

    g :: Zone -> Bool -> S.Set Zone -> S.Set Zone
    g zone bool = case bool of
        True -> S.insert zone
        False -> id

-- | The Zone, if any, which dislodged a unit in this Zone, without the
--   use of a convoy!
dislodgingZones
    :: M.Map Zone (Aligned Unit, SomeResolved OrderObject Typical)
    -> Zone
    -> S.Set Zone
dislodgingZones resolved zone = M.foldrWithKey f S.empty resolved
  where
    f :: Zone
      -> (Aligned Unit, SomeResolved OrderObject Typical)
      -> S.Set Zone
      -> S.Set Zone
    f zone' (aunit, SomeResolved (object, res)) = case object of
        MoveObject pt ->
            if Zone pt == zone
            then case (routes, res) of
                ([], Nothing) -> S.insert zone'
                _ -> id
            else id
          where
            routes = successfulConvoyRoutes (convoyRoutes resolved subject pt)
            subject = (alignedThing aunit, zoneProvinceTarget zone')
        _ -> id

{-
data AdjustPhaseOrderSet where
    AdjustPhaseOrderSet
        :: Maybe (Either (S.Set (Order Adjust Build)) (S.Set (Order Adjust Disband)))
        -> S.Set (Order Adjust Continue)
        -> AdjustPhaseOrderSet

validAdjustOrderSet
    :: GreatPower
    -> Occupation
    -> Control
    -> Maybe (Either (S.Set (Order Adjust Build)) (S.Set (Order Adjust Disband)))
validAdjustOrderSet greatPower occupation control
    -- All possible sets of build orders:
    | deficit < 0 = Just . Left $ allBuildOrderSets
    | deficit > 0 = Just . Right $ allDisbandOrderSets
    | otherwise = Nothing
  where
    deficit = supplyCentreDeficit greatPower occupation control
    -- To construct all build order sets, we take all subsets of the home
    -- supply centres of cardinality at most |deficit| and for each of these,
    -- make a subject for each kind of unit which can occupy that place. Note
    -- that in the case of special areas like St. Petersburg, we have 3 options!
    allBuildOrderSets = flattenSet $ (S.map . S.map) (\s -> Order (s, BuildObject)) allBuildOrderSubjects
    -- To construct all disband order sets, we take all subsets of this great
    -- power's subjects of cardinality exactly deficit.
    -- All subsets of the home supply centres, for each unit which can go
    -- there.
    allDisbandOrderSets = S.empty
    -- New strategy:
    --   We have all of the valid ProvinceTargets.
    --   For each of these, get the set of all pairs with units which can go
    --     there.
    --   Now pick from this set of sets; all ways to pick one from each set
    --     without going over |deficit|
    --allBuildOrderSubjects :: S.Set (S.Set Subject)
    --allBuildOrderSubjects = S.map (S.filter (\(unit, pt) -> S.member pt (unitCanOccupy unit))) . (S.map (setCartesianProduct (S.fromList [minBound..maxBound]))) $ allBuildOrderProvinceTargetSets
    allBuildOrderSubjects :: S.Set (S.Set Subject)
    allBuildOrderSubjects = foldr (\i -> S.union (pickSet i candidateSubjectSets)) S.empty [0..(abs deficit)]
    --allBuildOrderSubjects = S.filter ((flip (<=)) (abs deficit) . S.size) (powerSet candidateSubjects)
    --candidateSubjects :: S.Set Subject
    --candidateSubjects = S.filter (\(unit, pt) -> S.member pt (unitCanOccupy unit)) ((setCartesianProduct (S.fromList [minBound..maxBound])) candidateSupplyCentreSet)
    candidateSubjectSets :: S.Set (S.Set Subject)
    candidateSubjectSets = S.map (\pt -> S.filter (\(unit, pt) -> S.member pt (unitCanOccupy unit)) (setCartesianProduct (S.fromList [minBound..maxBound]) (S.singleton pt))) candidateSupplyCentreSet
-}

-- All continue order subjects which would make sense without any other orders
-- in context.
candidateContinueSubjects :: GreatPower -> Occupation -> S.Set Subject
candidateContinueSubjects greatPower = S.fromList . allSubjects (Just greatPower)

-- All disband order subjects which would make sense without any other orders
-- in context.
candidateDisbandSubjects :: GreatPower -> Occupation -> S.Set Subject
candidateDisbandSubjects greatPower = S.fromList . allSubjects (Just greatPower)

-- All build subjects which would make sense without any other adjust orders
-- in context: unoccupied home supply centre controlled by this great power
-- which the unit could legally occupy.
candidateBuildSubjects :: GreatPower -> Occupation -> Control -> S.Set Subject
candidateBuildSubjects greatPower occupation control =
    let candidateTargets = S.fromList $ candidateSupplyCentreTargets greatPower occupation control
        units :: S.Set Unit
        units = S.fromList $ [minBound..maxBound]
        candidateSubjects :: S.Set Subject
        candidateSubjects = setCartesianProduct units candidateTargets
    in  S.filter (\(u, pt) -> pt `S.member` unitCanOccupy u) candidateSubjects

candidateSupplyCentreTargets :: GreatPower -> Occupation -> Control -> [ProvinceTarget]
candidateSupplyCentreTargets greatPower occupation control = filter (not . (flip zoneOccupied) occupation . Zone) (controlledHomeSupplyCentreTargets greatPower control)

controlledHomeSupplyCentreTargets :: GreatPower -> Control -> [ProvinceTarget]
controlledHomeSupplyCentreTargets greatPower control = (controlledHomeSupplyCentres greatPower control >>= provinceTargets)

controlledHomeSupplyCentres :: GreatPower -> Control -> [Province]
controlledHomeSupplyCentres greatPower control = filter ((==) (Just greatPower) . (flip controller) control) (homeSupplyCentres greatPower)

homeSupplyCentres :: GreatPower -> [Province]
homeSupplyCentres greatPower = filter (isHome greatPower) supplyCentres

setCartesianProduct :: (Ord t, Ord s) => S.Set t -> S.Set s -> S.Set (t, s)
setCartesianProduct xs ys = S.foldr (\x -> S.union (S.map ((,) x) ys)) S.empty xs

powerSet :: Ord a => S.Set a -> S.Set (S.Set a)
powerSet = S.fold powerSetFold (S.singleton (S.empty))
  where
    powerSetFold :: Ord a => a -> S.Set (S.Set a) -> S.Set (S.Set a)
    powerSetFold elem pset = S.union (S.map (S.insert elem) pset) pset

flattenSet :: Ord a => S.Set (S.Set a) -> S.Set a
flattenSet = S.foldr S.union S.empty

setComplement :: Ord a => S.Set a -> S.Set a -> S.Set a
setComplement relativeTo = S.filter (not . (flip S.member) relativeTo)

-- Pick 1 thing from each of the sets to get a set of cardinality at most
-- n.
-- If there are m sets in the input set, you get a set of cardinality
-- at most m.
-- If n < 0 you get the empty set.
pickSet :: Ord a => Int -> S.Set (S.Set a) -> S.Set (S.Set a)
pickSet n sets
    | n <= 0 = S.singleton S.empty
    | otherwise = case S.size sets of
        0 -> S.empty
        m -> let xs = S.findMin sets
                 xss = S.delete xs sets
             in  case S.size xs of
                     0 -> pickSet n xss
                     l -> let rest = pickSet (n-1) xss
                          in  S.map (\(y, ys) -> S.insert y ys) (setCartesianProduct xs rest) `S.union` pickSet n xss

choose :: Ord a => Int -> S.Set a -> S.Set (S.Set a)
choose n set
    | n <= 0 = S.singleton (S.empty)
    | otherwise = case S.size set of
        0 -> S.empty
        m -> let x = S.findMin set
                 withoutX = choose n (S.delete x set)
                 withX = S.map (S.insert x) (choose (n-1) (S.delete x set))
             in  withX `S.union` withoutX

newtype Intersection t = Intersection [t]
newtype Union t = Union [t]

evalIntersection
    :: t
    -> (t -> t -> t)
    -> Intersection t
    -> t
evalIntersection empty intersect (Intersection is) = foldr intersect empty is

evalUnion
    :: t
    -> (t -> t -> t)
    -> Union t
    -> t
evalUnion empty union (Union us) = foldr union empty us

-- TBD better name, obviously.
-- No Functor superclass because, due to constraints on the element type, this
-- may not really be a Functor.
class SuitableFunctor (f :: * -> *) where
    type SuitableFunctorConstraint f :: * -> Constraint
    suitableEmpty :: f t
    suitableUnion :: SuitableFunctorConstraint f t => f t -> f t -> f t
    suitableIntersect :: SuitableFunctorConstraint f t => f t -> f t -> f t
    suitableMember :: SuitableFunctorConstraint f t => t -> f t -> Bool
    suitableFmap
        :: ( SuitableFunctorConstraint f t
           , SuitableFunctorConstraint f s
           )
        => (t -> s)
        -> f t
        -> f s
    suitablePure :: SuitableFunctorConstraint f t => t -> f t
    -- Instead of <*> we offer bundle, which can be used with
    -- suitableFmap and uncurry to emulate <*>.
    suitableBundle
        :: ( SuitableFunctorConstraint f t
           , SuitableFunctorConstraint f s
           )
        => f t
        -> f s
        -> f (t, s)
    suitableJoin :: SuitableFunctorConstraint f t => f (f t) -> f t
    suitableBind
        :: ( SuitableFunctorConstraint f t
           , SuitableFunctorConstraint f (f s)
           , SuitableFunctorConstraint f s
           )
        => f t
        -> (t -> f s)
        -> f s
    suitableBind x k = suitableJoin (suitableFmap k x)

instance SuitableFunctor [] where
    type SuitableFunctorConstraint [] = Eq
    suitableEmpty = []
    suitableUnion = union
    suitableIntersect = intersect
    suitableMember = elem
    suitableFmap = fmap
    suitableBundle = cartesianProduct
      where
        cartesianProduct :: (Eq a, Eq b) => [a] -> [b] -> [(a, b)]
        cartesianProduct xs ys = foldr (\x -> suitableUnion (fmap ((,) x) ys)) suitableEmpty xs
    suitablePure = pure
    suitableJoin = join

-- Shit, can't throw functions into a set!
-- Ok, so Ap is out; but can implement it with join instead.
instance SuitableFunctor S.Set where
    type SuitableFunctorConstraint S.Set = Ord
    suitableEmpty = S.empty
    suitableUnion = S.union
    suitableIntersect = S.intersection
    suitableMember = S.member
    suitableFmap = S.map
    suitableBundle = setCartesianProduct
    suitablePure = S.singleton
    suitableJoin = S.foldr suitableUnion suitableEmpty

-- Description of validity is here: given the prior arguments, produce a
-- tagged union of intersections for the next argument.
data ValidityCharacterization (g :: * -> *) (f :: * -> *) (k :: [*]) where
    VCNil
        :: ( SuitableFunctor f
           )
        => ValidityCharacterization g f '[]
    VCCons
        :: ( SuitableFunctor f
           , SuitableFunctorConstraint f t
           )
        => (ArgumentList Identity Identity ts -> TaggedIntersectionOfUnions g f t)
        -> ValidityCharacterization g f ts
        -> ValidityCharacterization g f (t ': ts)

validityCharacterizationTrans
    :: (forall s . g s -> h s)
    -> ValidityCharacterization g f ts
    -> ValidityCharacterization h f ts
validityCharacterizationTrans natTrans vc = case vc of
    VCNil -> VCNil
    VCCons f rest -> VCCons (taggedIntersectionOfUnionsTrans natTrans . f) (validityCharacterizationTrans natTrans rest)

-- Each thing which we intersect is endowed with a tag (the functor g).
type TaggedIntersectionOfUnions (g :: * -> *) (f :: * -> *) (t :: *) = Intersection (g (Union (f t)))

taggedIntersectionOfUnionsTrans
    :: (forall s . g s -> h s)
    -> TaggedIntersectionOfUnions g f t
    -> TaggedIntersectionOfUnions h f t
taggedIntersectionOfUnionsTrans trans iou = case iou of
    Intersection is -> Intersection (fmap trans is)

evalTaggedIntersectionOfUnions
    :: ( SuitableFunctor f
       , SuitableFunctorConstraint f t
       )
    => (forall s . g s -> s)
    -> TaggedIntersectionOfUnions g f t
    -> f t
evalTaggedIntersectionOfUnions exitG (Intersection is) =
    -- Must take special care here, since we have no identity under intersection.
    -- This is unfortunate, but necessary if we want to admit [] and Set as
    -- suitable functors!
    case is of
        [] -> suitableEmpty
        [x] -> evalUnion suitableEmpty suitableUnion (exitG x)
        x : xs -> suitableIntersect (evalUnion suitableEmpty suitableUnion (exitG x)) (evalTaggedIntersectionOfUnions exitG (Intersection xs))

checkTaggedIntersectionOfUnions
    :: ( SuitableFunctor f 
       , SuitableFunctorConstraint f t
       )
    => (forall s . g s -> s)
    -> (forall s . g s -> r)
    -> r
    -> (r -> r -> r)
    -> t
    -> TaggedIntersectionOfUnions g f t
    -> r
checkTaggedIntersectionOfUnions exitG inMonoid mempty mappend x (Intersection is) =
    foldr (\xs b -> if suitableMember x (evalUnion suitableEmpty suitableUnion (exitG xs)) then b else mappend (inMonoid xs) b) mempty is

data ArgumentList (g :: * -> *) (f :: * -> *) (k :: [*]) where
    ALNil :: ArgumentList g f '[]
    ALCons :: g (f t) -> ArgumentList g f ts -> ArgumentList g f (t ': ts)

type family Every (c :: * -> Constraint) (ts :: [*]) :: Constraint where
    Every c '[] = ()
    Every c (t ': ts) = (c t, Every c ts)

instance Every Show ts => Show (ArgumentList Identity Identity ts) where
    show al = case al of
        ALNil -> "ALNil"
        ALCons (Identity (Identity x)) rest -> "ALCons " ++ show x ++ " (" ++ show rest ++ ")"

instance Every Eq ts => Eq (ArgumentList Identity Identity ts) where
    x == y = case (x, y) of
        (ALNil, ALNil) -> True
        (ALCons (Identity (Identity x')) xs, ALCons (Identity (Identity y')) ys) -> x' == y' && xs == ys

instance (Every Ord ts, Every Eq ts) => Ord (ArgumentList Identity Identity ts) where
    x `compare` y = case (x, y) of
        (ALNil, ALNil) -> EQ
        (ALCons (Identity (Identity x')) xs, ALCons (Identity (Identity y')) ys) ->
            case x' `compare` y' of
                LT -> LT
                GT -> GT
                EQ -> xs `compare` ys

argListTrans
    :: (forall s . g s -> h s)
    -> ArgumentList g f ts
    -> ArgumentList h f ts
argListTrans natTrans argList = case argList of
    ALNil -> ALNil
    ALCons x rest -> ALCons (natTrans x) (argListTrans natTrans rest)

argListTrans1
    :: Functor g
    => (forall s . f s -> h s)
    -> ArgumentList g f ts
    -> ArgumentList g h ts
argListTrans1 natTrans argList = case argList of
    ALNil -> ALNil
    ALCons x rest -> ALCons (fmap natTrans x) (argListTrans1 natTrans rest)

-- This function is to use the VCCons constructor functions to build an f
-- coontaining all argument lists. Obviously, the SuitableFunctor must be
-- capable of carrying ArgumentList Identity Identity ts 
--
-- No, we should never have to union or intersect on f's containing
-- ArgumentList values, right?
evalValidityCharacterization
    :: ( SuitableFunctor f
       , ValidityCharacterizationConstraint f ts
       )
    => ValidityCharacterization Identity f ts
    -> f (ArgumentList Identity Identity ts)
evalValidityCharacterization vc = case vc of
    VCNil -> suitablePure ALNil
    VCCons next rest ->
        let rest' = evalValidityCharacterization rest
        in   suitableBind rest' $ \xs ->
             suitableBind (evalTaggedIntersectionOfUnions runIdentity (next xs)) $ \y ->
             suitablePure (ALCons (Identity (Identity y)) xs)

type family ValidityCharacterizationConstraint (f :: * -> *) (ts :: [*]) :: Constraint where
    ValidityCharacterizationConstraint f '[] = (
          SuitableFunctorConstraint f (ArgumentList Identity Identity '[])
        )
    ValidityCharacterizationConstraint f (t ': ts) = (
          SuitableFunctorConstraint f t
        , SuitableFunctorConstraint f (f t)
        , SuitableFunctorConstraint f (f (ArgumentList Identity Identity (t ': ts)))
        , SuitableFunctorConstraint f (t, ArgumentList Identity Identity ts)
        , SuitableFunctorConstraint f (ArgumentList Identity Identity (t ': ts))
        , SuitableFunctorConstraint f (ArgumentList Identity Identity ts)
        , ValidityCharacterizationConstraint f ts
        )

type Constructor ts t = ArgumentList Identity Identity ts -> t
type Deconstructor ts t = t -> ArgumentList Identity Identity ts

-- | VOC is an acronym for Valid Order Characterization
type VOC g f ts t = (Constructor ts t, Deconstructor ts t, ValidityCharacterization g f ts)

synthesize
    :: ( SuitableFunctor f
       , SuitableFunctorConstraint f (ArgumentList Identity Identity ts)
       , SuitableFunctorConstraint f t
       , ValidityCharacterizationConstraint f ts
       )
    => (forall s . g s -> Identity s)
    -> VOC g f ts t
    -> f t
synthesize trans (cons, _, vc) =
    let fArgList = evalValidityCharacterization (validityCharacterizationTrans trans vc)
    in  suitableFmap cons fArgList

analyze
    :: (forall s . g s -> s)
    -> (forall s . g s -> r)
    -> r
    -> (r -> r -> r)
    -> VOC g f ts t
    -> t
    -> r
analyze exitG inMonoid mempty mappend (_, uncons, vd) x =
    -- We unconstruct into an argument list, and now we must compare its
    -- members with the description
    let challenge = uncons x
    in  analyze' exitG inMonoid mempty mappend challenge vd
  where
    analyze'
        :: (forall s . g s -> s)
        -> (forall s . g s -> r)
        -> r
        -> (r -> r -> r)
        -> ArgumentList Identity Identity ts
        -> ValidityCharacterization g f ts
        -> r
    analyze' exitG inMonoid mempty mappend challenge vd = case (challenge, vd) of
            (ALNil, VCNil) -> mempty
            (ALCons (Identity (Identity x)) rest, VCCons f rest') ->
                let possibilities = f rest
                -- So here we are. possibilities is an intersection of unions.
                -- When evaluated (intersection taken) they give the set of all
                -- valid arguments here.
                -- BUT here we don't just take the intersection! No, we need
                -- to check membership in EACH of the intersectands, and if we
                -- find there's no membership, we must grab the tag and mappend
                -- it.
                    here = checkTaggedIntersectionOfUnions
                               exitG
                               inMonoid
                               mempty
                               mappend
                               x
                               possibilities
                    there = analyze' exitG inMonoid mempty mappend rest rest'
                in  here `mappend` there

-- Simple example case to see if things are working somewhat well.

type ValidityTag phase order = (,) (ValidityCriterion phase order)

type AdjustSetValidityTag = (,) (AdjustSetValidityCriterion)

moveVOC
    :: GreatPower
    -> Occupation
    -> VOC (ValidityTag Typical Move) S.Set '[ProvinceTarget, Subject] (Order Typical Move)
moveVOC greatPower occupation = (cons, uncons, vc)
  where
    vc :: ValidityCharacterization (ValidityTag Typical Move) S.Set '[ProvinceTarget, Subject]
    vc = VCCons (\(ALCons (Identity (Identity subject)) ALNil) -> Intersection [
              (MoveUnitCanOccupy, Union [unitCanOccupy (subjectUnit subject)])
            , (MoveReachable, Union [S.singleton (subjectProvinceTarget subject), validMoveAdjacency (Just occupation) subject])
            ])
        . VCCons (\ALNil -> Intersection [(MoveValidSubject, Union [S.fromList (allSubjects (Just greatPower) occupation)])])
        $ VCNil
    cons :: ArgumentList Identity Identity '[ProvinceTarget, Subject] -> Order Typical Move
    cons argList = case argList of
        ALCons (Identity (Identity pt)) (ALCons (Identity (Identity subject)) ALNil) ->
            Order (subject, MoveObject pt)
    uncons :: Order Typical Move -> ArgumentList Identity Identity '[ProvinceTarget, Subject]
    uncons (Order (subject, MoveObject pt)) =
        ALCons (return (return pt)) (ALCons (return (return subject)) ALNil)

supportVOC
    :: GreatPower
    -> Occupation
    -> VOC (ValidityTag Typical Support) S.Set '[Subject, ProvinceTarget, Subject] (Order Typical Support)
supportVOC greatPower occupation = (cons, uncons, vc)
  where
    vc :: ValidityCharacterization (ValidityTag Typical Support) S.Set '[Subject, ProvinceTarget, Subject]
    vc = -- Given a subject for the supporter, and a target for the support, we
         -- characterize every valid subject which can be supported.
         VCCons (\(ALCons (Identity (Identity pt)) (ALCons (Identity (Identity subject1)) ALNil)) -> Intersection [
              (SupportedCanDoMove, Union [S.filter (/= subject1) (validSupportSubjects occupation (subjectProvinceTarget subject1) pt)])
            ])
        -- Given a subject (the one who offers support), we check every place
        -- into which that supporter could offer support; that's every place
        -- where it could move without a convoy (or one of the special coasts
        -- of that place).
        . VCCons (\(ALCons (Identity (Identity subject)) ALNil) -> Intersection [
              (SupporterAdjacent, Union [validSupportTargets subject])
            ])
        . VCCons (\ALNil -> Intersection [(SupportValidSubject, Union [S.fromList (allSubjects (Just greatPower) occupation)])])
        $ VCNil
    cons :: ArgumentList Identity Identity '[Subject, ProvinceTarget, Subject] -> Order Typical Support
    cons argList = case argList of
        ALCons (Identity (Identity subject2)) (ALCons (Identity (Identity pt)) (ALCons (Identity (Identity subject1)) ALNil)) ->
            Order (subject1, SupportObject subject2 pt)
    uncons :: Order Typical Support -> ArgumentList Identity Identity '[Subject, ProvinceTarget, Subject]
    uncons order = case order of
        Order (subject1, SupportObject subject2 pt) ->
            ALCons (Identity (Identity subject2)) (ALCons (Identity (Identity pt)) (ALCons (Identity (Identity subject1)) ALNil))

convoyVOC
    :: GreatPower
    -> Occupation
    -> VOC (ValidityTag Typical Convoy) S.Set '[ProvinceTarget, Subject, Subject] (Order Typical Convoy)
convoyVOC greatPower occupation = (cons, uncons, vc)
  where
    vc :: ValidityCharacterization (ValidityTag Typical Convoy) S.Set '[ProvinceTarget, Subject, Subject]
    vc =  VCCons (\(ALCons (Identity (Identity convoyed)) (ALCons (Identity (Identity convoyer)) ALNil)) -> Intersection [
              (ConvoyValidConvoyTarget, Union [validConvoyTargets occupation convoyer convoyed])
            ])
        . VCCons (\(ALCons (Identity (Identity subject)) ALNil) -> Intersection [
              (ConvoyValidConvoySubject, Union [validConvoySubjects occupation])
            ])
        . VCCons (\ALNil -> Intersection [
              (ConvoyValidSubject, Union [validConvoyers (Just greatPower) occupation])
            ])
        $ VCNil
    cons :: ArgumentList Identity Identity '[ProvinceTarget, Subject, Subject] -> Order Typical Convoy
    cons al = case al of
        ALCons (Identity (Identity pt)) (ALCons (Identity (Identity convoyed)) (ALCons (Identity (Identity convoyer)) ALNil)) ->
            Order (convoyer, ConvoyObject convoyed pt)
    uncons :: Order Typical Convoy -> ArgumentList Identity Identity '[ProvinceTarget, Subject, Subject]
    uncons order = case order of
        Order (convoyer, ConvoyObject convoyed pt) ->
            ALCons (Identity (Identity pt)) (ALCons (Identity (Identity convoyed)) (ALCons (Identity (Identity convoyer)) ALNil))

surrenderVOC
    :: GreatPower
    -> Dislodgement
    -> VOC (ValidityTag Retreat Surrender) S.Set '[Subject] (Order Retreat Surrender)
surrenderVOC greatPower dislodgement = (cons, uncons, vc)
  where
    vc =  VCCons (\ALNil -> Intersection [
              (SurrenderValidSubject, Union [S.fromList (allSubjects (Just greatPower) dislodgement)])
            ])
        $ VCNil
    cons :: ArgumentList Identity Identity '[Subject] -> Order Retreat Surrender
    cons al = case al of
        ALCons (Identity (Identity subject)) ALNil ->
            Order (subject, SurrenderObject)
    uncons :: Order Retreat Surrender -> ArgumentList Identity Identity '[Subject]
    uncons order = case order of
        Order (subject, SurrenderObject) ->
            ALCons (Identity (Identity subject)) ALNil

withdrawVOC
    :: GreatPower
    -> M.Map Zone (Aligned Unit, SomeResolved OrderObject Typical)
    -> VOC (ValidityTag Retreat Withdraw) S.Set '[ProvinceTarget, Subject] (Order Retreat Withdraw)
withdrawVOC greatPower resolved = (cons, uncons, vc)
  where
    (dislodgement, occupation) = dislodgementAndOccupation resolved
    vc =  VCCons (\(ALCons (Identity (Identity subject)) ALNil) -> Intersection [
              (WithdrawAdjacent, Union [validMoveTargets Nothing subject])
            , (WithdrawNotDislodgingZone, Union [zoneSetToProvinceTargetSet $ S.difference setOfAllZones (dislodgingZones resolved (Zone (subjectProvinceTarget subject)))])
            , (WithdrawUncontestedZone, Union [zoneSetToProvinceTargetSet $ S.difference setOfAllZones (contestedZones resolved)])
            , (WithdrawUnoccupiedZone, Union [zoneSetToProvinceTargetSet $ S.difference setOfAllZones (occupiedZones occupation)])
            ])
        . VCCons (\ALNil -> Intersection [
              (WithdrawValidSubject, Union [S.fromList (allSubjects (Just greatPower) dislodgement)])
            ])
        $ VCNil
    cons :: ArgumentList Identity Identity '[ProvinceTarget, Subject] -> Order Retreat Withdraw
    cons al = case al of
        ALCons (Identity (Identity pt)) (ALCons (Identity (Identity subject)) ALNil) ->
            Order (subject, WithdrawObject pt)
    uncons :: Order Retreat Withdraw -> ArgumentList Identity Identity '[ProvinceTarget, Subject]
    uncons order = case order of
        Order (subject, WithdrawObject pt) ->
            ALCons (Identity (Identity pt)) (ALCons (Identity (Identity subject)) ALNil)

continueSubjectVOC
    :: GreatPower
    -> Occupation
    -> VOC (ValidityTag Adjust Continue) S.Set '[Subject] Subject
continueSubjectVOC greatPower occupation = (cons, uncons, vc)
  where
    vc :: ValidityCharacterization (ValidityTag Adjust Continue) S.Set '[Subject]
    vc =  VCCons (\ALNil -> Intersection [(ContinueValidSubject, Union [candidateContinueSubjects greatPower occupation])])
        $ VCNil
    cons :: ArgumentList Identity Identity '[Subject] -> Subject
    cons al = case al of
        ALCons (Identity (Identity subject)) ALNil -> subject
    uncons :: Subject -> ArgumentList Identity Identity '[Subject]
    uncons subject =
        ALCons (Identity (Identity subject)) ALNil

disbandSubjectVOC
    :: GreatPower
    -> Occupation
    -> VOC (ValidityTag Adjust Disband) S.Set '[Subject] Subject
disbandSubjectVOC greatPower occupation = (cons, uncons, vc)
  where
    vc :: ValidityCharacterization (ValidityTag Adjust Disband) S.Set '[Subject]
    vc =  VCCons (\ALNil -> Intersection [(DisbandValidSubject, Union [candidateDisbandSubjects greatPower occupation])])
        $ VCNil
    cons :: ArgumentList Identity Identity '[Subject] -> Subject
    cons al = case al of
        ALCons (Identity (Identity subject)) ALNil -> subject
    uncons :: Subject -> ArgumentList Identity Identity '[Subject]
    uncons subject =
        ALCons (Identity (Identity subject)) ALNil

-- Not a very useful factoring. Oh well, can make it sharper later if needed.
buildSubjectVOC
    :: GreatPower
    -> Occupation
    -> Control
    -> VOC (ValidityTag Adjust Build) S.Set '[Subject] Subject
buildSubjectVOC greatPower occupation control = (cons, uncons, vc)
  where
    vc :: ValidityCharacterization (ValidityTag Adjust Build) S.Set '[Subject]
    vc =  VCCons (\ALNil -> Intersection [(BuildValidSubject, Union [candidateBuildSubjects greatPower occupation control])])
        $ VCNil
    cons :: ArgumentList Identity Identity '[Subject] -> Subject
    cons al = case al of
        ALCons (Identity (Identity subject)) ALNil -> subject
    uncons :: Subject -> ArgumentList Identity Identity '[Subject]
    uncons subject =
        ALCons (Identity (Identity subject)) ALNil

-- Next up: given the set of adjust orders (special datatype or really
-- a set of SomeOrder?) give the valid subsets. Special datatype.
data AdjustSubjects = AdjustSubjects {
      buildSubjects :: S.Set Subject
    , disbandSubjects :: S.Set Subject
    , continueSubjects :: S.Set Subject
    }
    deriving (Eq, Ord, Show)

-- Here we assume that all of the subjects are valid according to
-- the characterizations with the SAME occupation, control, and great power.
--
-- Really though, what should be the output? Sets of SomeOrder are annoying,
-- because the Ord instance there is not trivial. Why not sets of
-- AdjustSubjects as we have here?
-- For 0 deficit, we give the singleton set of the AdjustSubjects in
-- which we make the build and disband sets empty.
-- For > 0 deficit, we take all deficit-element subsets of the disband
-- subjects, and for each of them we throw in the complement relative to
-- the continue subjects, and no build subjects.
-- For < 0 deficit, we take all (-deficit)-element or less subsets of the
-- build subjects, and for each of them we throw in the complement relative
-- to the continue subjects, and no disband subjects.
adjustSubjectsVOC
    :: GreatPower
    -> Occupation
    -> Control
    -> AdjustSubjects
    -> VOC AdjustSetValidityTag S.Set '[AdjustSubjects] AdjustSubjects
adjustSubjectsVOC greatPower occupation control subjects = (cons, uncons, vc)
  where
    deficit = supplyCentreDeficit greatPower occupation control
    vc :: ValidityCharacterization AdjustSetValidityTag S.Set '[AdjustSubjects]
    vc =  VCCons (\ALNil -> tiu)
        $ VCNil
    cons :: ArgumentList Identity Identity '[AdjustSubjects] -> AdjustSubjects
    cons al = case al of
        ALCons (Identity (Identity x)) ALNil -> x
    uncons :: AdjustSubjects -> ArgumentList Identity Identity '[AdjustSubjects]
    uncons x =
        ALCons (Identity (Identity x)) ALNil
    tiu :: TaggedIntersectionOfUnions AdjustSetValidityTag S.Set AdjustSubjects
    tiu | deficit > 0 = let disbandSets = choose deficit disbands
                            pairs = S.map (\xs -> (xs, continues `S.difference` xs)) disbandSets
                            valids :: S.Set AdjustSubjects
                            valids = S.map (\(disbands, continues) -> AdjustSubjects S.empty disbands continues) pairs
                        in  Intersection [(RequiredNumberOfDisbands, Union (fmap S.singleton (S.toList valids)))]
        | deficit < 0 = let buildSetsUnzoned :: [S.Set (S.Set Subject)]
                            buildSetsUnzoned = fmap (\n -> choose n builds) [0..(-deficit)] 
                            -- buildSetsUnzoned is not quite what we want; its
                            -- member sets may include subjects of the same
                            -- zone. A fleet in Marseilles and an army in
                            -- Marseilles, for instance. To remedy this, we
                            -- set-map each one to and from ZonedSubjectDull,
                            -- whose Eq/Ord instances ignore the unit and uses
                            -- zone-equality. Then, to rule out duplicate sets,
                            -- we do this again with the ZonedSubjectSharp
                            -- type, which uses zone-equality but does not
                            -- ignore the unit. This ensure that, for instance,
                            -- the sets {(Fleet, Marseilles)} and
                            -- {(Army, Marseilles)} can coexist in buildSets.
                            buildSets :: [S.Set (S.Set Subject)]
                            buildSets =
                                fmap
                                    (S.map (S.map zonedSubjectSharp) . (S.map (S.map (ZonedSubjectSharp . zonedSubjectDull) . (S.map ZonedSubjectDull))))
                                    buildSetsUnzoned
                            pairs :: [S.Set (S.Set Subject, S.Set Subject)]
                            pairs = (fmap . S.map) (\xs -> (xs, continues `S.difference` xs)) buildSets
                            valids :: [S.Set AdjustSubjects]
                            valids = (fmap . S.map) (\(builds, continues) -> AdjustSubjects builds S.empty continues) pairs
                        in  Intersection [(AdmissibleNumberOfBuilds, Union valids)]
        | otherwise = Intersection [(OnlyContinues, Union [S.singleton (AdjustSubjects S.empty S.empty continues)])]
    builds = buildSubjects subjects
    disbands = disbandSubjects subjects
    continues = continueSubjects subjects
