{-|
Module      : Diplomacy.OrderValidation
Description : Definition of order validation
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)

A valid order is one which, in the absence of other orders to compete with it,
would pass and lead one sane game state to another sane game state.

TODO remove the use of Occupation and the InvalidSubject reason. At validation
time, we should assume every subject is valid, as this can be enforced by
judicious choice of data types for representing a diplomacy board: 

  Map Zone (Aligned Unit, SomeOrderObject phase)

-}

{-# LANGUAGE AutoDeriveTypeable #-}
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

  {-
    OrderValidation
  , InvalidReason(..)
  , SomeInvalidReason(..)

  , validateMove
  , validateSupport
  , validateConvoy
  , validateSurrender
  , validateWithdraw
  , validateDisband
  , validateBuild
  , validateContinue
  -}

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
import Diplomacy.Occupation
import Diplomacy.SupplyCentreDefecit
import Diplomacy.Valid
import Diplomacy.OrderResolution

import Debug.Trace

-- Each one of these constructors is associated with a set.
data ValidityCriterion (phase :: Phase) where

    MoveUnitCanOccupy :: ValidityCriterion Typical
    MoveReachable :: ValidityCriterion Typical

    SupportValidSubject :: ValidityCriterion Typical
    SupportValidTarget :: ValidityCriterion Typical

    ConvoyFleetInWater :: ValidityCriterion Typical
    ConvoyValidSubject :: ValidityCriterion Typical
    ConvoyValidTarget :: ValidityCriterion Typical

    WithdrawAdjacent :: ValidityCriterion Retreat
    WithdrawUnoccupied :: ValidityCriterion Retreat
    WithdrawUncontested :: ValidityCriterion Retreat
    WithdrawNotAttacking :: ValidityCriterion Retreat

    CorrectNumberOfDisbands :: ValidityCriterion Adjust
    AdmissibleNumberOfBuilds :: ValidityCriterion Adjust

deriving instance Eq (ValidityCriterion phase)
deriving instance Ord (ValidityCriterion phase)

-- | All ProvinceTargets which a unit can legally occupy.
unitCanOccupy :: Unit -> [ProvinceTarget]
unitCanOccupy unit = case unit of
    Army -> fmap Normal . filter (not . isWater) $ [minBound..maxBound]
    Fleet -> do
        pr <- [minBound..maxBound]
        guard (not (isInland pr))
        case provinceCoasts pr of
            [] -> return $ Normal pr
            xs -> fmap Special xs

-- | All places to which a unit could possibly move (without regard for
--   occupation rules as specified by unitCanOccupy).
--   The Occupation parameter is needed to determine which convoys are possible.
--   If it's nothing, we don't consider convoy routes.
validMoveAdjacency :: Maybe Occupation -> Subject -> [ProvinceTarget]
validMoveAdjacency occupation subject = case subjectUnit subject of
    Army -> case occupation of
        Nothing -> neighbours pt
        Just o -> neighbours pt ++ (fmap Normal (convoyTargets o pr))
    Fleet -> do
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

convoyTargets :: Occupation -> Province -> [Province]
convoyTargets occupation = fmap fst . convoyPaths occupation

validMoveTargets
    :: Maybe Occupation
    -> Subject
    -> [ProvinceTarget]
validMoveTargets maybeOccupation subject =
    (validMoveAdjacency maybeOccupation subject)
    `intersect`
    (unitCanOccupy (subjectUnit subject))

-- | Valid support targets are any place where this subject could move without
--   a convoy (this excludes the subject's province target).
validSupportTargets
    :: Subject
    -> [ProvinceTarget]
validSupportTargets = validMoveAdjacency Nothing

-- | Valid support targets depend upon the support subject AND its chosen
--   target! For example, if we choose to support into Brest, then a fleet
--   in the Tyrrhenian Sea cannot be the support subject.
validSupportSubjects
    :: Occupation
    -> Subject
    -> ProvinceTarget
    -> [Subject]
validSupportSubjects occupation subject target = M.foldWithKey f [] occupation
  where
    pt = subjectProvinceTarget subject
    f zone aunit =
        if elem target (validMoveTargets (Just occupation) subject')
        then (:) subject'
        else id
      where
        subject' = (alignedThing aunit, zoneProvinceTarget zone)

-- | Subjects which could act as convoyers: fleets in water.
validConvoyers
    :: Maybe GreatPower
    -> Occupation
    -> [Subject]
validConvoyers greatPower = M.foldWithKey f []
  where
    f zone aunit = case unit of
        Fleet -> if    isWater (ptProvince pt)
                    && (  greatPower == Nothing
                       || greatPower == Just (alignedGreatPower aunit)
                       )
                 then (:) (unit, pt)
                 else id
        _ -> id
      where
        pt = zoneProvinceTarget zone
        unit = alignedThing aunit

-- | Subjects which could be convoyed: armies on coasts.
validConvoySubjects
    :: Occupation
    -> [Subject]
validConvoySubjects = M.foldWithKey f []
  where
    f zone aunit = if unit == Army && isCoastal (ptProvince pt)
                   then (:) (unit, pt)
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
    -> [ProvinceTarget]
validConvoyTargets occupation subjectConvoyer subjectConvoyed =
    let allConvoyPaths = convoyPaths occupation prConvoyed
        convoyPathsWithThis = filter (elem prConvoyer . snd) allConvoyPaths
    in  fmap (Normal . fst) convoyPathsWithThis
  where
    prConvoyer = ptProvince (subjectProvinceTarget subjectConvoyer)
    prConvoyed = ptProvince (subjectProvinceTarget subjectConvoyed)



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
    suitableBundle = cartesianProduct
      where
        cartesianProduct :: (Ord t, Ord s) => S.Set t -> S.Set s -> S.Set (t, s)
        cartesianProduct xs ys = S.foldr (\x -> suitableUnion (S.map ((,) x) ys)) suitableEmpty xs
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

type VOD g f ts t = (Constructor ts t, Deconstructor ts t, ValidityCharacterization g f ts)

type family ArgumentListTuple (ts :: [*]) :: * where
    ArgumentListTuple '[] = ()
    ArgumentListTuple '[t] = Identity t
    ArgumentListTuple (t ': ts) = (t, ArgumentListTuple ts)

toArgumentListTuple :: ArgumentList Identity Identity ts -> ArgumentListTuple ts
toArgumentListTuple al = case al of
    ALNil -> ()
    ALCons (Identity (Identity x)) rest -> case rest of
        ALNil -> Identity x
        ALCons _ _ -> (x, toArgumentListTuple rest)

{-
class FromArgumentListTuple (ts :: [*]) where
    fromArgumentListTuple :: ArgumentListTuple ts -> ArgumentList Identity Identity ts

instance FromArgumentListTuple '[] where
    fromArgumentListTuple tuple = case tuple of
        () -> ALNil

instance FromArgumentListTuple '[x] where
    fromArgumentListTuple (Identity x) = ALCons (Identity (Identity x)) ALNil

instance FromArgumentListTuple xs => FromArgumentListTuple (x ': xs) where
    fromArgumentListTuple tuple = case tuple of
        -- Must show that xs is not '[] !!!!
        (x, rest) -> case rest of
            () -> ALCons (Identity (Identity x)) ALNil
            Identity y -> ALCons (Identity (Identity x)) (ALCons (Identity (Identity y)) ALNil)
            tuple' -> ALCons (Identity (Identity x)) (fromArgumentListTuple tuple')
-}

synthesize
    :: ( SuitableFunctor f
       , SuitableFunctorConstraint f (ArgumentList Identity Identity ts)
       , SuitableFunctorConstraint f t
       , ValidityCharacterizationConstraint f ts
       )
    => (forall s . g s -> Identity s)
    -> VOD g f ts t
    -> f t
synthesize trans (cons, _, vc) =
    let fArgList = evalValidityCharacterization (validityCharacterizationTrans trans vc)
    in  suitableFmap cons fArgList

analyze
    :: (forall s . g s -> s)
    -> (forall s . g s -> r)
    -> r
    -> (r -> r -> r)
    -> VOD g f ts t
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

type StringTag = (,) String

moveVOD
    :: GreatPower
    -> Occupation
    -> VOD StringTag S.Set '[ProvinceTarget, Subject] (Order Typical Move)
moveVOD greatPower occupation = (cons, uncons, vc)
  where
    vc :: ValidityCharacterization StringTag S.Set '[ProvinceTarget, Subject]
    vc = VCCons (\(ALCons (Identity (Identity subject)) ALNil) -> Intersection [
              ("UnitCanOccupy", Union [S.fromList (unitCanOccupy (subjectUnit subject))])
            , ("MoveAdjacent", Union [S.singleton (subjectProvinceTarget subject), S.fromList (validMoveAdjacency (Just occupation) subject)])
            ])
        . VCCons (\ALNil -> Intersection [("ValidSubject", Union [S.fromList (allSubjects (Just greatPower) occupation)])])
        $ VCNil
    cons :: ArgumentList Identity Identity '[ProvinceTarget, Subject] -> Order Typical Move
    cons argList = case argList of
        ALCons (Identity (Identity pt)) (ALCons (Identity (Identity subject)) ALNil) ->
            Order (subject, MoveObject pt)
    uncons :: Order Typical Move -> ArgumentList Identity Identity '[ProvinceTarget, Subject]
    uncons (Order (subject, MoveObject pt)) =
        ALCons (return (return pt)) (ALCons (return (return subject)) ALNil)

supportVOD
    :: GreatPower
    -> Occupation
    -> VOD StringTag S.Set '[Subject, ProvinceTarget, Subject] (Order Typical Support)
supportVOD greatPower occupation = (cons, uncons, vc)
  where
    vc :: ValidityCharacterization StringTag S.Set '[Subject, ProvinceTarget, Subject]
    vc = -- Given a subject for the supporter, and a target for the support, we
         -- characterize every valid subject which can be supported.
         VCCons (\(ALCons (Identity (Identity pt)) (ALCons (Identity (Identity subject1)) ALNil)) -> Intersection [
              ("SupportedAdjacent", Union [S.filter (/= subject1) (S.fromList (validSupportSubjects occupation subject1 pt))])
              -- Would be nice to be able to declare "not in this set" without
              -- having to construct the complement: instead of
              --   xs `intersection` (complement ys)
              -- we would say
              --   xs `without` ys
              -- but I suppose it's not necessary.
            ])
        -- Given a subject for the supporter, we check every place into which
        -- that supporter could offer support; that's every place where it
        -- could move without a convoy.
        . VCCons (\(ALCons (Identity (Identity subject)) ALNil) -> Intersection [
              ("SupporterUnitCanOccupy", Union [S.fromList (unitCanOccupy (subjectUnit subject))])
            , ("SupporterAdjacent", Union [S.fromList (validSupportTargets subject)])
            ])
        . VCCons (\ALNil -> Intersection [("ValidSubject", Union [S.fromList (allSubjects (Just greatPower) occupation)])])
        $ VCNil
    cons :: ArgumentList Identity Identity '[Subject, ProvinceTarget, Subject] -> Order Typical Support
    cons argList = case argList of
        ALCons (Identity (Identity subject2)) (ALCons (Identity (Identity pt)) (ALCons (Identity (Identity subject1)) ALNil)) ->
            Order (subject1, SupportObject subject2 pt)
    uncons :: Order Typical Support -> ArgumentList Identity Identity '[Subject, ProvinceTarget, Subject]
    uncons order = case order of
        Order (subject1, SupportObject subject2 pt) ->
            ALCons (Identity (Identity subject2)) (ALCons (Identity (Identity pt)) (ALCons (Identity (Identity subject1)) ALNil))

convoyVOD
    :: GreatPower
    -> Occupation
    -> VOD StringTag S.Set '[ProvinceTarget, Subject, Subject] (Order Typical Convoy)
convoyVOD greatPower occupation = (cons, uncons, vc)
  where
    vc :: ValidityCharacterization StringTag S.Set '[ProvinceTarget, Subject, Subject]
    vc =  VCCons (\(ALCons (Identity (Identity convoyed)) (ALCons (Identity (Identity convoyer)) ALNil)) -> Intersection [
              ("ValidyConvoyTarget", Union [S.fromList (validConvoyTargets occupation convoyer convoyed)])
            ])
        . VCCons (\(ALCons (Identity (Identity subject)) ALNil) -> Intersection [
              ("ValidConvoySubject", Union [S.fromList (validConvoySubjects occupation)])
            ])
        . VCCons (\ALNil -> Intersection [
              ("ValidSubject", Union [S.fromList (validConvoyers (Just greatPower) occupation)])
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

{-
type OrderValidation phase order =
    Order phase order -> Maybe (InvalidReason phase order)

-- | Enumeration of reasons for the invalidity of an order.
data InvalidReason (phase :: Phase) (order :: OrderType) where

    -- | The subject specified is inconsistent with the state of the game: the
    --   issuing country does not have a unit of the given type in the given
    --   province target.
    InvalidSubject
      :: InvalidReason phase orderType

    -- | The move cannot be achieved, even via convoy. A move to any
    --   nonadjacent inland province, for example; or from an inland province
    --   to any nonadjacent province; or a fleet moving to any nonadjacent
    --   or inland province, etc.
    MoveImpossible
      :: InvalidReason Typical Move

    -- | The support is directed to or from the supporting unit's province.
    --   For example, the following two are invalid for this reason:
    --
    --     A Munich S A Munch - Berlin
    --     A Paris S A Brest - Paris
    --
    SupportSelf
      :: InvalidReason Typical Support

    -- | The unit to support is not present.
    SupportedUnitNotPresent
      :: InvalidReason Typical Support

    -- | The supported unit could not legally do the move that would be
    --   supported.
    SupportedCouldNotDoMove
      :: InvalidReason Typical Move -- ^ The reason why the supported unit
                                    --   cannot do the move
      -> InvalidReason Typical Support

    -- | The supporting unit could not attack the target of support.
    --   This eliminates support through convoys.
    SupporterCouldNotDoMove
      :: InvalidReason Typical Support

    ConvoyerIsNotFleet :: InvalidReason Typical Convoy

    ConvoyingIsNotArmy :: InvalidReason Typical Convoy

    ConvoyerNotInWater :: InvalidReason Typical Convoy

    ConvoyingNotOnCoastal :: InvalidReason Typical Convoy

    ConvoyingUnitNotPresent :: InvalidReason Typical Convoy

    -- | The withdraw destination is not directly adjacent to the province
    --   from which the unit withdraws.
    WithdrawNonAdjacent
      :: InvalidReason Retreat Withdraw

    -- | The withdraw destination is the province from which the withdrawing
    --   unit was dislodged.
    WithdrawIntoAttackingProvince
      :: InvalidReason Retreat Withdraw

    WithdrawIntoContestedArea :: InvalidReason Retreat Withdraw

    -- | The withdraw destination is occupied.
    WithdrawIntoOccupiedProvince
      :: InvalidReason Retreat Withdraw

    -- | The unit to be built cannot legally occupy the destination province.
    BuildUnitCannotOccupy
      :: InvalidReason Adjust Build

    -- | The unit is to be built in a non-home supply centre. This includes
    --   provinces which are not supply centres at all, like
    --     not (home && supplyCentre)
    --   rather than
    --     (not home) && supplyCentre
    BuildNotInHomeSupplyCentre
      :: InvalidReason Adjust Build

    -- | The issuing power does not have enough supply centres to allow for a
    --   new unit.
    BuildInsufficientSupplyCentres
      :: InvalidReason Adjust Build

deriving instance Show (InvalidReason phase order)
deriving instance Eq (InvalidReason phase order)

data SomeInvalidReason (phase :: Phase) where
    SomeInvalidReason :: InvalidReason phase order -> SomeInvalidReason phase

deriving instance Show (SomeInvalidReason phase)

valid :: OrderValidation phase order
valid = const Nothing

-- | True implies invalid.
implies
  :: (a -> Bool)
  -> b
  -> (a -> Maybe b)
implies fbool invalid order = case fbool order of
    True -> Just invalid
    False -> Nothing

-- | True implies valid.
orElse
  :: (a -> Bool)
  -> b
  -> (a -> Maybe b)
orElse fbool = implies (not . fbool)

also
  :: (a -> Maybe b)
  -> (a -> Maybe b)
  -> (a -> Maybe b)
also left right = (<|>) <$> left <*> right

validateAs
  :: (Order phase2 order2 -> Order phase1 order1)
  -> (InvalidReason phase1 order1 -> InvalidReason phase2 order2)
  -> OrderValidation phase1 order1
  -> OrderValidation phase2 order2
validateAs forder invalid validation1 order = case validation1 (forder order) of
    Nothing -> Nothing
    Just reason -> Just (invalid reason)

-- | Validation for the subject of an order.
validateSubject :: GreatPower -> Occupation -> OrderValidation phase order
validateSubject greatPower occupation order =
    case occupies alignedUnit provinceTarget occupation of
        True -> Nothing
        False -> Just InvalidSubject
  where
    subject = orderSubject order
    alignedUnit = align (subjectUnit subject) greatPower
    provinceTarget = subjectProvinceTarget subject

-- * Principal validations

-- | Validation for a move order.
validateMove :: GreatPower -> Occupation -> OrderValidation Typical Move
validateMove greatPower occupation =
           validateSubject greatPower occupation
    `also` validateMoveAdjacency

-- | Validation for a support order.
validateSupport :: GreatPower -> Occupation -> OrderValidation Typical Support
validateSupport greatPower occupation =
           validateSubject greatPower occupation
    `also` validateSupportSelf
    `also` validateSupportedUnitPresent occupation
    `also` validateSupporterCanDoMove
    `also` validateSupportedCanDoMove

-- | Validation for a convoy order.
--   NB we do NOT check for convoys which are impossible, like a fleet in
--   the Black Sea convoying Finland to Sweden.
validateConvoy :: GreatPower -> Occupation -> OrderValidation Typical Convoy
validateConvoy greatPower occupation =
           validateSubject greatPower occupation
    `also` validateConvoyerIsFleet
    `also` validateConvoyingIsArmy
    `also` validateConvoyerInWater
    `also` validateConvoyingOnCoastal
    `also` validateConvoyingUnitPresent occupation

-- | Validation for a surrender order.
validateSurrender :: GreatPower -> Occupation -> OrderValidation Retreat Surrender
validateSurrender = validateSubject

-- | Validation for a withdraw order.
validateWithdraw :: GreatPower -> Occupation -> TypicalResolution -> OrderValidation Retreat Withdraw
validateWithdraw greatPower occupation resolved =
           validateSubject greatPower occupation
    `also` validateWithdrawNonAdjacent
    `also` validateWithdrawIntoOccupiedProvince occupation
    `also` validateWithdrawIntoAttackingProvince resolved
    `also` validateWithdrawIntoContestedArea resolved

-- | Validation for a disband order.
validateDisband :: GreatPower -> Occupation -> OrderValidation Adjust Disband
validateDisband = validateSubject

-- | Validation for a build order.
validateBuild :: GreatPower -> SupplyCentreDefecit -> OrderValidation Adjust Build
validateBuild greatPower defecit =
           validateBuildUnitCannotOccupy
    `also` validateBuildInHomeSupplyCentre greatPower
    `also` validateBuildRespectsDefecit defecit

validateContinue :: GreatPower -> Occupation -> OrderValidation Adjust Continue
validateContinue = validateSubject

-- * Sub-validations used to define principal validations.

validateSupportSelf :: OrderValidation Typical Support
validateSupportSelf =
    supportSelf
    `implies`
    SupportSelf
  where
    supportSelf order =
        let supportAt = subjectProvinceTarget (orderSubject (order))
        in     supportAt == supportTarget (orderObject order)
            || supportAt == subjectProvinceTarget (supportedSubject (orderObject order))

validateSupportedCanDoMove :: OrderValidation Typical Support
validateSupportedCanDoMove order = fmap SupportedCouldNotDoMove (validateMoveAdjacency (makeMove order))
  where
    makeMove :: Order Typical Support -> Order Typical Move
    makeMove order =
        let SupportObject subject to = orderObject order
        --  It doesn't matter that the move we construct may have a different
        --  issuing great power than the move being supported, because the
        --  validity of a move is independent of the issuing power!
        in  Order (subject, MoveObject to)

validateSupportedUnitPresent :: Occupation -> OrderValidation Typical Support
validateSupportedUnitPresent occupation =
    supportedUnitNotPresent occupation
    `implies`
    SupportedUnitNotPresent

supportedUnitNotPresent :: Occupation -> Order Typical Support -> Bool
supportedUnitNotPresent occupation order =
    not (unitOccupies unit provinceTarget occupation)
  where
    SupportObject supportedSubject to = orderObject order
    unit = subjectUnit supportedSubject
    provinceTarget = subjectProvinceTarget supportedSubject

validateConvoyerIsFleet :: OrderValidation Typical Convoy
validateConvoyerIsFleet =
    convoyerIsFleet
    `orElse`
    ConvoyerIsNotFleet
  where
    convoyerIsFleet order = case subjectUnit (orderSubject order) of
        Fleet -> True
        Army -> False

validateConvoyingIsArmy :: OrderValidation Typical Convoy
validateConvoyingIsArmy =
    convoyingIsArmy
    `orElse`
    ConvoyingIsNotArmy
  where
    convoyingIsArmy order = case orderObject order of
        ConvoyObject (Army, _) _ -> True
        _ -> False

validateConvoyerInWater :: OrderValidation Typical Convoy
validateConvoyerInWater =
    convoyerInWater
    `orElse`
    ConvoyerNotInWater
  where
    convoyerInWater = isWater . ptProvince . subjectProvinceTarget . orderSubject

validateConvoyingOnCoastal :: OrderValidation Typical Convoy
validateConvoyingOnCoastal =
    convoyingOnCoastal
    `orElse`
    ConvoyingNotOnCoastal
  where
    convoyingOnCoastal = isCoastal . ptProvince . subjectProvinceTarget . convoySubject . orderObject

validateConvoyingUnitPresent :: Occupation -> OrderValidation Typical Convoy
validateConvoyingUnitPresent occupation =
    convoyingUnitNotPresent
    `implies`
    ConvoyingUnitNotPresent
  where
    convoyingUnitNotPresent order = not (unitOccupies (unit order) (provinceTarget order) occupation)
    unit = subjectUnit . orderSubject
    provinceTarget = subjectProvinceTarget . orderSubject

validateWithdrawIntoOccupiedProvince :: Occupation -> OrderValidation Retreat Withdraw
validateWithdrawIntoOccupiedProvince occupation =
    withdrawIntoOccupiedProvince occupation
    `implies`
    WithdrawIntoOccupiedProvince

withdrawIntoOccupiedProvince :: Occupation -> Order Retreat Withdraw -> Bool
withdrawIntoOccupiedProvince occupation order = zoneOccupied (Zone target) occupation
  where
    WithdrawObject target = orderObject order

validateWithdrawIntoAttackingProvince
    :: TypicalResolution
    -> OrderValidation Retreat Withdraw
validateWithdrawIntoAttackingProvince resolved =
    withdrawIntoAttackingProvince resolved
    `implies`
    WithdrawIntoAttackingProvince

withdrawIntoAttackingProvince
    :: TypicalResolution
    -> Order Retreat Withdraw
    -> Bool
withdrawIntoAttackingProvince resolved order = case lookupWithKey (Zone target) resolved of
    Nothing -> False
    Just (key, (aunit, SomeResolved (someOrderObject, resolution))) ->
        -- Must pattern match on the resolution, to know whether the move
        -- succeeded (Nothing means no failure reason, so it succeeded).
        -- We also must check the successful convoy routes. If this move has at
        -- least one, then the withdraw is OK.
        case (resolution, someOrderObject) of
            (Nothing, MoveObject to) ->
                if Zone to /= Zone from
                then False
                else case successfulConvoyRoutes (convoyRoutes resolved (alignedThing aunit, zoneProvinceTarget key) to) of
                    [] -> True
                    _ -> False
            _ -> False
  where
    WithdrawObject target = orderObject order
    from = subjectProvinceTarget (orderSubject order)

-- | A withdraw into an area where a standoff occurred is considered invalid.
validateWithdrawIntoContestedArea :: TypicalResolution -> OrderValidation Retreat Withdraw
validateWithdrawIntoContestedArea resolution =
    invalidWithdrawIntoContestedArea resolution
    `implies`
    WithdrawIntoContestedArea

invalidWithdrawIntoContestedArea :: TypicalResolution -> Order Retreat Withdraw -> Bool
invalidWithdrawIntoContestedArea resolution order = M.fold folder False resolution
  where
    folder :: (Aligned Unit, SomeResolved OrderObject Typical) -> Bool -> Bool
    folder (aunit, SomeResolved (object, thisResolution)) b = b || case object of
        MoveObject movingTo ->
            if Zone movingTo /= Zone (withdrawTarget (orderObject order))
            then False
            else case thisResolution of
                -- careful: a move that's overpowered by a move from its
                -- destination does not contest the area.
                Just (MoveOverpowered overpowerers) ->
                    all (\x -> Zone movingTo /= Zone (subjectProvinceTarget (alignedThing x))) (toList overpowerers)
                Just (MoveBounced _) -> True
                _ -> False
        _ -> False

validateWithdrawNonAdjacent :: OrderValidation Retreat Withdraw
validateWithdrawNonAdjacent =
    invalidWithdrawNonAdjacent
    `implies`
    WithdrawNonAdjacent

invalidWithdrawNonAdjacent :: Order Retreat Withdraw -> Bool
invalidWithdrawNonAdjacent order = unitCannotWithdrawFromTo unit from to
  where
    unit = subjectUnit (orderSubject order)
    from = subjectProvinceTarget (orderSubject order)
    WithdrawObject to = orderObject order

validateBuildUnitCannotOccupy :: OrderValidation Adjust Build
validateBuildUnitCannotOccupy = buildUnitCannotOccupy `implies` BuildUnitCannotOccupy

validateBuildInHomeSupplyCentre :: GreatPower -> OrderValidation Adjust Build
validateBuildInHomeSupplyCentre greatPower = notInHomeSupplyCentre greatPower `implies` BuildNotInHomeSupplyCentre

validateBuildRespectsDefecit :: SupplyCentreDefecit -> OrderValidation Adjust Build
validateBuildRespectsDefecit i = const (i >= 0) `implies` BuildInsufficientSupplyCentres

buildUnitCannotOccupy :: Order Adjust Build -> Bool
buildUnitCannotOccupy order = unitCannotOccupy unit target
  where
    unit = subjectUnit (orderSubject order)
    target = subjectProvinceTarget (orderSubject order)

notInHomeSupplyCentre :: GreatPower -> Order Adjust Build -> Bool
notInHomeSupplyCentre greatPower order = not (supplyCentre province && isHome greatPower province)
  where
    province = ptProvince (subjectProvinceTarget (orderSubject order))

validateMoveAdjacency :: OrderValidation Typical Move
validateMoveAdjacency = invalidMoveAdjacency `implies` MoveImpossible

invalidMoveAdjacency :: Order Typical Move -> Bool
invalidMoveAdjacency order =
       unitCannotMoveFromTo unit from to
  where
    unit = subjectUnit (orderSubject order)
    from = subjectProvinceTarget (orderSubject order)
    MoveObject to = orderObject order

validateSupporterCanDoMove :: OrderValidation Typical Support
validateSupporterCanDoMove =
    invalidSupporterCanDoMove
    `implies`
    SupporterCouldNotDoMove

invalidSupporterCanDoMove :: Order Typical Support -> Bool
invalidSupporterCanDoMove order = unitCannotSupportFromTo unit from to
  where
    unit = subjectUnit (orderSubject order)
    from = subjectProvinceTarget (orderSubject order)
    SupportObject _ to = orderObject order

-- * Some utility functions.

-- | True if and only if the unit cannot not legally occupy the province target.
--   This keeps armies off the water, fleets away from inland, armies from
--   choosing a coastline on a multi-coast province, and fleets from
--   landing on a multi-coast province without choosing a coast.
unitCannotOccupy :: Unit -> ProvinceTarget -> Bool
unitCannotOccupy unit provinceTarget =
    case unit of
        Army -> isWater (ptProvince provinceTarget) || isSpecial provinceTarget
        Fleet ->    isInland (ptProvince provinceTarget)
                 || (  not (null (provinceCoasts (ptProvince (provinceTarget))))
                    && not (isSpecial provinceTarget)
                    )

-- | True if and only if the unit cannot legally move from the first province
--   target to the second. This *does* account for occupation rules as
--   determined by @unitCannotOccupy@. This *does not* account for convoy
--   routes; use @unitCannotConvoyFromTo@ for that.
unitCannotMoveFromTo :: Unit -> ProvinceTarget -> ProvinceTarget -> Bool
unitCannotMoveFromTo unit from to = unitCannotOccupy unit to || case unit of
    -- Armies can convoy, so we consider all from/to pairs to be valid.
    -- In the future, we may want to check that there is a convoy route between
    -- these two places.
    -- Since it's easy, we just get the case of non-adjacent provinces in which
    -- at least one is inland (making a convoy impossible).
    Army ->    not (isSameOrNeighbour to from)
            && (isInland (ptProvince from) || isInland (ptProvince to))
    Fleet -> if isCoastal (ptProvince from) && isCoastal (ptProvince to)
             then not (isSameOrNeighbour to from) || null (commonCoasts from to)
             else not (isSameOrNeighbour to from)

-- | True if and only if the unit stationed at the first province target
--   cannot legally support a move to the second province target.
--   This rules out support-through-convoy, and is careful to permit support
--   by coastal fleets to either coast of a multi-coast province.
unitCannotSupportFromTo :: Unit -> ProvinceTarget -> ProvinceTarget -> Bool
unitCannotSupportFromTo unit from to = case unit of
    -- We use adjacent of Provinces rather than neighbourhood of ProvinceTargets
    -- because an army can support into any special coast.
    Army -> isWater (ptProvince to) || not (isSameOrAdjacent (ptProvince to) (ptProvince from))
    -- The decision for fleet supports depends upon whether special coastlines
    -- are involved.
    Fleet ->
        if isInland (ptProvince to)
        then True
        else if isWater (ptProvince to)
        then unitCannotMoveFromTo unit from to
        -- When supporting into a coastal ProvinceTarget,
        -- unitCannotMoveFromTo is too strict: we must try it for every coast
        -- of the province.
        else all (unitCannotMoveFromTo Fleet from) (provinceTargetCluster to)

-- | True if and only if the unit cannot legally withdraw from the first
--   province target to the second. This is the same as for unconvoyed
--   movement.
unitCannotWithdrawFromTo :: Unit -> ProvinceTarget -> ProvinceTarget -> Bool
unitCannotWithdrawFromTo = unitCannotMoveFromTo

-- | True if and only if the unit cannot legally convoy from the first
--   province target to the second. This rules out any convoy of a fleet, as
--   well as convoys which form loops.
unitCannotConvoyFromTo :: Unit -> ProvinceTarget -> ProvinceTarget -> Bool
unitCannotConvoyFromTo unit from to = case unit of
    Army -> elem to (convoyNeighbours from)
    Fleet -> True
  where
    convoyNeighbours from = filter (/= from) (waterReachables from)
-}
