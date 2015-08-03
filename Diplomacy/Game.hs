{-|
Module      : Diplomacy.Game
Description : 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Diplomacy.Game (

    Game(..)
  , Round(..)
  , RoundStatus(..)
  , Status(..)
  , TypicalRound(..)
  , RetreatRound(..)
  , AdjustRound(..)
  , NextRound
  , RoundPhase
  , RoundOrderConstructor
  , roundToInt

  , gameZonedOrders
  , gameZonedResolvedOrders
  , gameOccupation
  , gameDislodged
  , gameControl
  , gameTurn
  , gameRound
  , gameSeason
  , issueOrders
  , resolve
  , continue
  , newGame
  , showGame

  ) where

import Control.Applicative
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (sortBy, intersperse)
import Diplomacy.Turn
import Diplomacy.Season
import Diplomacy.GreatPower
import Diplomacy.Aligned
import Diplomacy.Unit
import Diplomacy.Order
import Diplomacy.OrderObject
import Diplomacy.Phase
import Diplomacy.Province
import Diplomacy.Zone
import Diplomacy.Occupation
import Diplomacy.Dislodgement
import Diplomacy.Control
import Diplomacy.Subject
import Diplomacy.SupplyCentreDefecit
import Diplomacy.OrderResolution
import Diplomacy.OrderValidation

data Round where
    RoundOne :: Round
    RoundTwo :: Round
    RoundThree :: Round
    RoundFour :: Round
    RoundFive :: Round

deriving instance Show Round
deriving instance Enum Round
deriving instance Bounded Round

roundToInt :: Round -> Int
roundToInt = fromEnum

data RoundStatus where
    RoundUnresolved :: RoundStatus
    RoundResolved :: RoundStatus

deriving instance Show RoundStatus

data Status (roundStatus :: RoundStatus) where
    Unresolved :: Status RoundUnresolved
    Resolved :: Status RoundResolved

type family RoundOrderConstructor (roundStatus :: RoundStatus) :: Phase -> * where
    RoundOrderConstructor RoundUnresolved = SomeOrderObject
    RoundOrderConstructor RoundResolved = SomeResolved OrderObject

data TypicalRound (round :: Round) where
    TypicalRoundOne :: TypicalRound RoundOne
    TypicalRoundTwo :: TypicalRound RoundThree

deriving instance Show (TypicalRound round)

nextRetreatRound :: TypicalRound round -> RetreatRound (NextRound round)
nextRetreatRound typicalRound = case typicalRound of
    TypicalRoundOne -> RetreatRoundOne
    TypicalRoundTwo -> RetreatRoundTwo

data RetreatRound (round :: Round) where
    RetreatRoundOne :: RetreatRound RoundTwo
    RetreatRoundTwo :: RetreatRound RoundFour

deriving instance Show (RetreatRound round)

data AdjustRound (round :: Round) where
    AdjustRound :: AdjustRound RoundFive

deriving instance Show (AdjustRound round)

type family NextRound (round :: Round) :: Round where
    NextRound RoundOne = RoundTwo
    NextRound RoundTwo = RoundThree
    NextRound RoundThree = RoundFour
    NextRound RoundFour = RoundFive
    NextRound RoundFive = RoundOne

type family RoundPhase (round :: Round) :: Phase where
    RoundPhase RoundOne = Typical
    RoundPhase RoundTwo = Retreat
    RoundPhase RoundThree = Typical
    RoundPhase RoundFour = Retreat
    RoundPhase RoundFive = Adjust

data Game (round :: Round) (roundStatus :: RoundStatus) where

    TypicalGame
        :: TypicalRound round
        -> Status roundStatus
        -> Turn
        -> M.Map Zone (Aligned Unit, RoundOrderConstructor roundStatus Typical)
        -> Control
        -> Game round roundStatus

    RetreatGame
        :: RetreatRound round
        -> Status roundStatus
        -> Turn
        -> TypicalResolution
        -- ^ Resolutions of the previous typical phase.
        -> M.Map Zone (Aligned Unit, RoundOrderConstructor roundStatus Retreat)
        -- ^ Dislodged units, which have orders.
        -> Occupation
        -> Control
        -> Game round roundStatus

    AdjustGame
        :: AdjustRound round
        -> Status roundStatus
        -> Turn
        -> M.Map Zone (Aligned Unit, RoundOrderConstructor roundStatus Adjust)
        -> Control
        -> Game round roundStatus

newGame :: Game RoundOne RoundUnresolved
newGame = TypicalGame TypicalRoundOne Unresolved firstTurn zonedOrders thisControl
  where
    zonedOrders = M.mapWithKey giveDefaultOrder thisOccupation

    giveDefaultOrder
        :: Zone
        -> Aligned Unit
        -> (Aligned Unit, SomeOrderObject Typical)
    giveDefaultOrder zone aunit = (aunit, SomeOrderObject (MoveObject (zoneProvinceTarget zone)))

    thisOccupation =

          occupy (Normal London) (Just (align Fleet England))
        . occupy (Normal Edinburgh) (Just (align Fleet England))
        . occupy (Normal Liverpool) (Just (align Army England))

        . occupy (Normal Brest) (Just (align Fleet France))
        . occupy (Normal Paris) (Just (align Army France))
        . occupy (Normal Marseilles) (Just (align Army France))

        . occupy (Normal Venice) (Just (align Army Italy))
        . occupy (Normal Rome) (Just (align Army Italy))
        . occupy (Normal Naples) (Just (align Fleet Italy))

        . occupy (Normal Kiel) (Just (align Fleet Germany))
        . occupy (Normal Berlin) (Just (align Army Germany))
        . occupy (Normal Munich) (Just (align Army Germany))

        . occupy (Normal Vienna) (Just (align Army Austria))
        . occupy (Normal Budapest) (Just (align Army Austria))
        . occupy (Normal Trieste) (Just (align Fleet Austria))

        . occupy (Normal Warsaw) (Just (align Army Russia))
        . occupy (Normal Moscow) (Just (align Army Russia))
        . occupy (Special StPetersburgSouth) (Just (align Fleet Russia))
        . occupy (Normal Sevastopol) (Just (align Fleet Russia))

        . occupy (Normal Constantinople) (Just (align Army Turkey))
        . occupy (Normal Smyrna) (Just (align Army Turkey))
        . occupy (Normal Ankara) (Just (align Fleet Turkey))

        $ emptyOccupation

    -- Initial control: everybody controls their home supply centres.
    thisControl :: Control
    thisControl = foldr (\(power, province) -> control province (Just power)) emptyControl controlList
      where
        controlList :: [(GreatPower, Province)]
        controlList = [ (power, province) | power <- greatPowers, province <- filter (isHome power) supplyCentres ]
        greatPowers :: [GreatPower]
        greatPowers = [minBound..maxBound]

showGame :: Game round roundStatus -> String
showGame game = concat . intersperse "\n" $ [
      showGameMetadata game
    , "****"
    , middle
    , "****"
    , showControl (gameControl game)
    ]
  where
    middle = case game of
        TypicalGame _ Unresolved _ _ _ -> showZonedOrders (gameZonedOrders game)
        RetreatGame _ Unresolved _ _ _ _ _ -> showZonedOrders (gameZonedOrders game)
        AdjustGame _ Unresolved _ _ _ -> showZonedOrders (gameZonedOrders game)
        TypicalGame _ Resolved _ _ _ -> showZonedResolvedOrders (gameZonedResolvedOrders game)
        RetreatGame _ Resolved _ _ _ _ _ -> showZonedResolvedOrders (gameZonedResolvedOrders game)
        AdjustGame _ Resolved _ _ _ -> showZonedResolvedOrders (gameZonedResolvedOrders game)

showGameMetadata :: Game round roundStatus -> String
showGameMetadata game = concat . intersperse "\n" $ [
      "Year: " ++ show year
    , "Season: " ++ show season
    , "Phase: " ++ show phase
    ]
  where
    year = 1900 + turnToInt (gameTurn game)
    season = gameSeason game
    phase = gamePhase game

showOccupation :: Occupation -> String
showOccupation = concat . intersperse "\n" . M.foldWithKey foldShowAlignedUnit []
  where
    foldShowAlignedUnit zone aunit b =
        concat [show provinceTarget, ": ", show greatPower, " ", show unit] : b
      where
        provinceTarget = zoneProvinceTarget zone
        greatPower = alignedGreatPower aunit
        unit = alignedThing aunit

showZonedOrders :: M.Map Zone (Aligned Unit, SomeOrderObject phase) -> String
showZonedOrders = concat . intersperse "\n" . M.foldWithKey foldShowOrder []
  where
    foldShowOrder zone (aunit, SomeOrderObject object) b =
        concat [show provinceTarget, ": ", show greatPower, " ", show unit, " ", objectString] : b
      where
        provinceTarget = zoneProvinceTarget zone
        greatPower = alignedGreatPower aunit
        unit = alignedThing aunit
        objectString = case object of
            MoveObject pt ->
                if pt == zoneProvinceTarget zone
                then "hold"
                else "move to " ++ show pt
            SupportObject subj pt -> concat ["support ", show supportedUnit, " at ", show supportedPt, " into ", show pt]
              where
                supportedUnit = subjectUnit subj
                supportedPt = subjectProvinceTarget subj
            ConvoyObject subj pt -> concat ["convoy ", show convoyedUnit, " from ", show convoyedFrom, " to ", show pt]
              where
                convoyedUnit = subjectUnit subj
                convoyedFrom = subjectProvinceTarget subj
            SurrenderObject -> "surrender"
            WithdrawObject pt -> "withdraw to " ++ show pt
            DisbandObject -> "disband"
            BuildObject -> "build"
            ContinueObject -> "continue"

showZonedResolvedOrders :: M.Map Zone (Aligned Unit, SomeResolved OrderObject phase) -> String
showZonedResolvedOrders = concat . intersperse "\n" . M.foldWithKey foldShowResolvedOrder []
  where
    foldShowResolvedOrder
        :: Zone
        -> (Aligned Unit, SomeResolved OrderObject phase)
        -> [String]
        -> [String]
    foldShowResolvedOrder zone (aunit, SomeResolved (object, resolution)) b =
        concat [show provinceTarget, ": ", show greatPower, " ", show unit, " ", objectString, " ", resolutionString] : b
      where
        provinceTarget = zoneProvinceTarget zone
        greatPower = alignedGreatPower aunit
        unit = alignedThing aunit
        objectString = case object of
            MoveObject pt ->
                if pt == zoneProvinceTarget zone
                then "hold"
                else "move to " ++ show pt
            SupportObject subj pt -> concat ["support ", show supportedUnit, " at ", show supportedPt, " into ", show pt]
              where
                supportedUnit = subjectUnit subj
                supportedPt = subjectProvinceTarget subj
            ConvoyObject subj pt -> concat ["convoy ", show convoyedUnit, " from ", show convoyedFrom, " to ", show pt]
              where
                convoyedUnit = subjectUnit subj
                convoyedFrom = subjectProvinceTarget subj
            SurrenderObject -> "surrender"
            WithdrawObject pt -> "withdraw to " ++ show pt
            DisbandObject -> "disband"
            BuildObject -> "build"
            ContinueObject -> "continue"
        resolutionString = case resolution of
            Nothing -> "✓"
            Just reason -> "✗ " ++ show reason

showControl :: Control -> String
showControl = concat . intersperse "\n" . M.foldWithKey foldShowControl []
  where
    foldShowControl province greatPower b = concat [show province, ": ", show greatPower] : b

gameStatus :: Game round roundStatus -> Status roundStatus
gameStatus game = case game of
    TypicalGame _ x _ _ _ -> x
    RetreatGame _ x _ _ _ _ _ -> x
    AdjustGame _ x _ _ _ -> x

gameZonedOrders
    :: Game round RoundUnresolved
    -> M.Map Zone (Aligned Unit, SomeOrderObject (RoundPhase round))
gameZonedOrders game = case game of
    TypicalGame TypicalRoundOne _ _ x _ -> x
    TypicalGame TypicalRoundTwo _ _ x _ -> x
    RetreatGame RetreatRoundOne _ _ _ x _ _ -> x
    RetreatGame RetreatRoundTwo _ _ _ x _ _ -> x
    AdjustGame AdjustRound _ _ x _ -> x

gameZonedResolvedOrders
    :: Game round RoundResolved
    -> M.Map Zone (Aligned Unit, SomeResolved OrderObject (RoundPhase round))
gameZonedResolvedOrders game = case game of
    TypicalGame TypicalRoundOne _ _ x _ -> x
    TypicalGame TypicalRoundTwo _ _ x _ -> x
    RetreatGame RetreatRoundOne _ _ _ x _ _ -> x
    RetreatGame RetreatRoundTwo _ _ _ x _ _ -> x
    AdjustGame AdjustRound _ _ x _ -> x

gameOccupation :: Game round roundStatus -> Occupation
gameOccupation game = case game of
    TypicalGame _ _ _ zonedOrders _ -> M.map fst zonedOrders
    RetreatGame _ _ _ _ _ x _ -> x
    AdjustGame _ Unresolved _ zonedOrders _ -> M.mapMaybe selectDisbandOrContinue zonedOrders
      where
        selectDisbandOrContinue :: (Aligned Unit, SomeOrderObject Adjust) -> Maybe (Aligned Unit)
        selectDisbandOrContinue (aunit, SomeOrderObject object) = case object of
            DisbandObject -> Just aunit
            ContinueObject -> Just aunit
            _ -> Nothing
    AdjustGame _ Resolved _ zonedOrders _ -> M.mapMaybe selectBuildOrContinue zonedOrders
      where
        selectBuildOrContinue :: (Aligned Unit, SomeResolved OrderObject Adjust) -> Maybe (Aligned Unit)
        selectBuildOrContinue (aunit, SomeResolved (object, _)) = case object of
            BuildObject -> Just aunit
            ContinueObject -> Just aunit
            _ -> Nothing

gameDislodged
    :: (RoundPhase round ~ Retreat)
    => Game round RoundUnresolved
    -> M.Map Zone (Aligned Unit)
gameDislodged game = case game of
    RetreatGame _ Unresolved _ _ zonedOrders _ _ -> M.map fst zonedOrders

gameResolved
    :: (RoundPhase round ~ Retreat)
    => Game round RoundUnresolved
    -> M.Map Zone (Aligned Unit, SomeResolved OrderObject Typical)
gameResolved game = case game of
    RetreatGame _ _ _ x _ _ _ -> x

gameControl :: Game round roundStatus -> Control
gameControl game = case game of
    TypicalGame _ _ _ _ c -> c
    RetreatGame _ _ _ _ _ _ c -> c
    AdjustGame _ _ _ _ c -> c

gameTurn :: Game round roundStatus -> Turn
gameTurn game = case game of
    TypicalGame _ _ t _ _ -> t
    RetreatGame _ _ t _ _ _ _ -> t
    AdjustGame _ _ t _ _ -> t

gameRound :: Game round roundStatus -> Round
gameRound game = case game of
    TypicalGame TypicalRoundOne _ _ _ _ -> RoundOne
    TypicalGame TypicalRoundTwo _ _ _ _ -> RoundThree
    RetreatGame RetreatRoundOne _ _ _ _ _ _ -> RoundTwo
    RetreatGame RetreatRoundTwo _ _ _ _ _ _ -> RoundFour
    AdjustGame AdjustRound _ _ _ _ -> RoundFive

gameSeason :: Game round roundStatus -> Season
gameSeason game = case game of
    TypicalGame TypicalRoundOne _ _ _ _ -> Spring
    RetreatGame RetreatRoundOne _ _ _ _ _ _ -> Spring
    TypicalGame TypicalRoundTwo _ _ _ _ -> Fall
    RetreatGame RetreatRoundTwo _ _ _ _ _ _ -> Fall
    AdjustGame _ _ _ _ _ -> Winter

gamePhase :: Game round roundStatus -> Phase
gamePhase game = case game of
    TypicalGame _ _ _ _ _ -> Typical
    RetreatGame _ _ _ _ _ _ _ -> Retreat
    AdjustGame _ _ _ _ _ -> Adjust


-- Can only issue orders for one great power.
-- Must offer the ability to issue more than one order, else issuing
-- adjust phase orders would be impossible.
--
-- TBD the return type.
-- There may be more than one invalid order given. We must associate each
-- order with the set of criteria which it fails to meet, and give back the
-- next game. If any order is invalid, no orders shall be issued.
-- Of course, for the adjust phase, things are slightly different. Not only
-- is each order associated with its set of invalid reasons, but the set itself
-- has a set of reasons!

type family ValidateOrdersOutput (phase :: Phase) :: * where
    ValidateOrdersOutput Typical = M.Map Zone (Aligned Unit, SomeOrderObject Typical, S.Set (SomeValidityCriterion Typical))
    ValidateOrdersOutput Retreat = M.Map Zone (Aligned Unit, SomeOrderObject Retreat, S.Set (SomeValidityCriterion Retreat))
    ValidateOrdersOutput Adjust = (M.Map Zone (Aligned Unit, SomeOrderObject Adjust, S.Set (SomeValidityCriterion Adjust)), M.Map GreatPower (S.Set AdjustSetValidityCriterion))

-- | The game given as the second component of the return value will differ
--   from the input game only if all orders are valid.
--   NB for adjust phase we wipe all build orders; that's because there's
--   no way to explicitly remove a build order by overwriting it with some
--   other order.
issueOrders
    :: forall round .
       M.Map Zone (Aligned Unit, SomeOrderObject (RoundPhase round))
    -> Game round RoundUnresolved
    -> (ValidateOrdersOutput (RoundPhase round), Game round RoundUnresolved)
issueOrders orders game =
    let nextGame = case game of
            AdjustGame AdjustRound _ _ _ _ -> issueOrdersUnsafe orders (removeBuildOrders game)
            _ -> issueOrdersUnsafe orders game
        validation :: ValidateOrdersOutput (RoundPhase round)
        allValid :: Bool
        (validation, allValid) = case game of
            TypicalGame TypicalRoundOne _ _ _ _ ->
                let validation = validateOrders orders game
                    invalids = M.fold pickInvalids S.empty validation
                in  (validation, S.null invalids)
            TypicalGame TypicalRoundTwo _ _ _ _ ->
                let validation = validateOrders orders game
                    invalids = M.fold pickInvalids S.empty validation
                in  (validation, S.null invalids)
            RetreatGame RetreatRoundOne _ _ _ _ _ _ ->
                let validation = validateOrders orders game
                    invalids = M.fold pickInvalids S.empty validation
                in  (validation, S.null invalids)
            RetreatGame RetreatRoundTwo _ _ _ _ _ _ ->
                let validation = validateOrders orders game
                    invalids = M.fold pickInvalids S.empty validation
                in  (validation, S.null invalids)
            AdjustGame AdjustRound _ _ _ _ ->
                let validation = validateOrders orders game
                    invalids = M.fold pickInvalids S.empty (fst validation)
                    adjustSetInvalids = M.fold S.union S.empty (snd validation)
                in  (validation, S.null invalids && S.null adjustSetInvalids)
    in  if allValid
        then (validation, nextGame)
        else (validation, game)
  where
    pickInvalids
        :: (Aligned Unit, SomeOrderObject phase, S.Set (SomeValidityCriterion phase))
        -> S.Set (SomeValidityCriterion phase)
        -> S.Set (SomeValidityCriterion phase)
    pickInvalids (_, _, x) = S.union x

validateOrders
    :: forall round .
       M.Map Zone (Aligned Unit, SomeOrderObject (RoundPhase round))
    -> Game round RoundUnresolved
    -> ValidateOrdersOutput (RoundPhase round)
validateOrders orders game = case game of
    -- The form of validation depends upon the game phase:
    -- - Typical and Retreat orders are validated independently, so we can
    --   express validation as a fold.
    -- - Adjust orders are validated independently and then ensemble.
    TypicalGame TypicalRoundOne _ _ _ _ -> M.mapWithKey (validateOrderTypical game) orders
    TypicalGame TypicalRoundTwo _ _ _ _ -> M.mapWithKey (validateOrderTypical game) orders
    RetreatGame RetreatRoundOne _ _ _ _ _ _ -> M.mapWithKey (validateOrderRetreat game) orders
    RetreatGame RetreatRoundTwo _ _ _ _ _ _ -> M.mapWithKey (validateOrderRetreat game) orders
    AdjustGame AdjustRound _ _ _ _ ->
        let independent = M.mapWithKey (validateOrderSubjectAdjust game) orders
            ensemble = validateOrdersAdjust game orders
        in  (independent, ensemble)
  where

    validateOrderTypical
        :: forall round .
           ( RoundPhase round ~ Typical )
        => Game round RoundUnresolved
        -> Zone
        -> (Aligned Unit, SomeOrderObject (RoundPhase round))
        -> (Aligned Unit, SomeOrderObject (RoundPhase round), S.Set (SomeValidityCriterion Typical))
    validateOrderTypical game zone (aunit, SomeOrderObject object) =
        (aunit, SomeOrderObject object, validation)
      where
        validation = case object of
            MoveObject _ -> analyze snd (S.singleton . SomeValidityCriterion . fst) S.empty S.union (moveVOC greatPower occupation) (Order (subject, object))
            SupportObject _ _ -> analyze snd (S.singleton . SomeValidityCriterion . fst) S.empty S.union (supportVOC greatPower occupation) (Order (subject, object))
            ConvoyObject _ _ -> analyze snd (S.singleton . SomeValidityCriterion . fst) S.empty S.union (convoyVOC greatPower occupation) (Order (subject, object))
        occupation = gameOccupation game
        greatPower = alignedGreatPower aunit
        unit = alignedThing aunit
        subject = (unit, zoneProvinceTarget zone)

    validateOrderRetreat
        :: forall round .
           ( RoundPhase round ~ Retreat )
        => Game round RoundUnresolved
        -> Zone
        -> (Aligned Unit, SomeOrderObject (RoundPhase round)) 
        -> (Aligned Unit, SomeOrderObject (RoundPhase round), S.Set (SomeValidityCriterion Retreat))
    validateOrderRetreat game zone (aunit, SomeOrderObject object) =
        (aunit, SomeOrderObject object, validation)
      where
        validation = case object of
            SurrenderObject -> analyze snd (S.singleton . SomeValidityCriterion . fst) S.empty S.union (surrenderVOC greatPower dislodgement) (Order (subject, object))
            WithdrawObject _ -> analyze snd (S.singleton . SomeValidityCriterion . fst) S.empty S.union (withdrawVOC greatPower resolved) (Order (subject, object))
        occupation = gameOccupation game
        resolved = gameResolved game
        dislodgement = gameDislodged game
        greatPower = alignedGreatPower aunit
        unit = alignedThing aunit
        subject = (unit, zoneProvinceTarget zone)

    -- The above two functions give us single-order validations for typical
    -- and retreat phases... for adjust we need single-order validation and
    -- also order-set validation. But then, the return value type of
    -- validateOrders must surely depend upon the phase, no? We want to
    -- associate each input order with its set of failed criteria, and then
    -- associate the set itself with its failed criteria. So we'll want
    -- a type family.
    validateOrderSubjectAdjust
        :: forall round .
           ( RoundPhase round ~ Adjust )
        => Game round RoundUnresolved
        -> Zone
        -> (Aligned Unit, SomeOrderObject (RoundPhase round))
        -> (Aligned Unit, SomeOrderObject (RoundPhase round), S.Set (SomeValidityCriterion Adjust))
    validateOrderSubjectAdjust game zone (aunit, SomeOrderObject object) =
        (aunit, SomeOrderObject object, validation)
      where
        validation = case object of
            ContinueObject -> analyze snd (S.singleton . SomeValidityCriterion . fst) S.empty S.union (continueSubjectVOC greatPower occupation) subject
            DisbandObject -> analyze snd (S.singleton . SomeValidityCriterion . fst) S.empty S.union (disbandSubjectVOC greatPower occupation) subject
            BuildObject -> analyze snd (S.singleton . SomeValidityCriterion . fst) S.empty S.union (buildSubjectVOC greatPower occupation control) subject
        occupation = gameOccupation game
        control = gameControl game
        greatPower = alignedGreatPower aunit
        unit = alignedThing aunit
        subject = (unit, zoneProvinceTarget zone)

    -- Here we partition the subjects by GreatPower, because each power's set of
    -- adjust orders must be analyzed ensemble to determine whether it makes
    -- sense (enough disbands/not too many builds for instance).
    validateOrdersAdjust
        :: forall round .
           ( RoundPhase round ~ Adjust )
        => Game round RoundUnresolved
        -> M.Map Zone (Aligned Unit, SomeOrderObject (RoundPhase round))
        -> M.Map GreatPower (S.Set AdjustSetValidityCriterion)
    validateOrdersAdjust game orders = M.mapWithKey validation adjustSetsByGreatPower
      where
        validation
            :: GreatPower
            -> AdjustSubjects
            -> S.Set AdjustSetValidityCriterion
        validation greatPower subjects = analyze snd (S.singleton . fst) S.empty S.union (adjustSubjectsVOC greatPower occupation control subjects) subjects
        adjustSetsByGreatPower :: M.Map GreatPower AdjustSubjects
        adjustSetsByGreatPower = M.foldWithKey pickSubject M.empty orders
        pickSubject
            :: Zone
            -> (Aligned Unit, SomeOrderObject (RoundPhase round))
            -> M.Map GreatPower AdjustSubjects
            -> M.Map GreatPower AdjustSubjects
        pickSubject zone (aunit, SomeOrderObject object) = case object of
            ContinueObject -> M.alter (alterContinue subject) greatPower
            BuildObject -> M.alter (alterBuild subject) greatPower
            DisbandObject -> M.alter (alterDisband subject) greatPower
          where
            subject = (alignedThing aunit, zoneProvinceTarget zone)
            greatPower = alignedGreatPower aunit
        alterContinue
            :: Subject
            -> Maybe AdjustSubjects
            -> Maybe AdjustSubjects
        alterContinue subject x = Just $ case x of
            Nothing -> AdjustSubjects S.empty S.empty (S.singleton subject)
            Just x' -> x' { continueSubjects = S.insert subject (continueSubjects x') }
        alterBuild
            :: Subject
            -> Maybe AdjustSubjects
            -> Maybe AdjustSubjects
        alterBuild subject x = Just $ case x of
            Nothing -> AdjustSubjects (S.singleton subject) S.empty S.empty
            Just x' -> x' { buildSubjects = S.insert subject (buildSubjects x') }
        alterDisband
            :: Subject
            -> Maybe AdjustSubjects
            -> Maybe AdjustSubjects
        alterDisband subject x = Just $ case x of
            Nothing -> AdjustSubjects S.empty (S.singleton subject) S.empty
            Just x' -> x' { disbandSubjects = S.insert subject (disbandSubjects x') }
        occupation = gameOccupation game
        control = gameControl game

-- | Issue orders without validating them. Do not use this with orders which
--   have not been validated!
issueOrdersUnsafe
    :: forall round .
       M.Map Zone (Aligned Unit, SomeOrderObject (RoundPhase round))
    -> Game round RoundUnresolved
    -> Game round RoundUnresolved
issueOrdersUnsafe validOrders game = M.foldWithKey issueOrderUnsafe game validOrders
  where
    issueOrderUnsafe
        :: forall round .
           Zone
        -> (Aligned Unit, SomeOrderObject (RoundPhase round))
        -> Game round RoundUnresolved
        -> Game round RoundUnresolved
    issueOrderUnsafe zone (aunit, someObject) game = case game of
        TypicalGame TypicalRoundOne s t zonedOrders v -> TypicalGame TypicalRoundOne s t (insertOrder zonedOrders) v
        TypicalGame TypicalRoundTwo s t zonedOrders v -> TypicalGame TypicalRoundTwo s t (insertOrder zonedOrders) v
        RetreatGame RetreatRoundOne s t res zonedOrders o c -> RetreatGame RetreatRoundOne s t res (insertOrder zonedOrders) o c
        RetreatGame RetreatRoundTwo s t res zonedOrders o c -> RetreatGame RetreatRoundTwo s t res (insertOrder zonedOrders) o c
        AdjustGame AdjustRound s t zonedOrders c -> AdjustGame AdjustRound s t (insertOrder zonedOrders) c
      where
        insertOrder
            :: M.Map Zone (Aligned Unit, SomeOrderObject (RoundPhase round))
            -> M.Map Zone (Aligned Unit, SomeOrderObject (RoundPhase round))
        insertOrder = M.alter (const (Just (aunit, someObject))) zone

removeBuildOrders
    :: (RoundPhase round ~ Adjust)
    => Game round RoundUnresolved
    -> Game round RoundUnresolved
removeBuildOrders game = case game of
    AdjustGame AdjustRound s t zonedOrders c ->
        let zonedOrders' = M.filter (not . isBuild) zonedOrders
        in  AdjustGame AdjustRound s t zonedOrders' c
  where
    isBuild :: (Aligned Unit, SomeOrderObject Adjust) -> Bool
    isBuild (_, SomeOrderObject object) = case object of
        BuildObject -> True
        _ -> False

resolve
    :: Game round RoundUnresolved
    -> Game round RoundResolved
resolve game = case game of
    TypicalGame round _ turn zonedOrders control ->
        TypicalGame round Resolved turn (typicalResolution zonedOrders) control
    RetreatGame round _ turn previousResolution zonedOrders occupation control ->
        RetreatGame round Resolved turn previousResolution (retreatResolution zonedOrders) occupation control
    AdjustGame round _ turn zonedOrders control ->
        AdjustGame round Resolved turn (adjustResolution zonedOrders) control

continue
    :: Game round RoundResolved
    -> Game (NextRound round) RoundUnresolved
continue game = case game of

    TypicalGame round _ turn zonedResolvedOrders thisControl ->
        RetreatGame (nextRetreatRound round) Unresolved turn zonedResolvedOrders nextZonedOrders occupation thisControl
      where
        -- Give every dislodged unit a surrender order.
        nextZonedOrders :: M.Map Zone (Aligned Unit, SomeOrderObject Retreat)
        nextZonedOrders = M.map giveDefaultRetreatOrder dislodgement

        giveDefaultRetreatOrder
            :: Aligned Unit
            -> (Aligned Unit, SomeOrderObject Retreat)
        giveDefaultRetreatOrder aunit = (aunit, SomeOrderObject object)
          where
            object = SurrenderObject

        (dislodgement, occupation) = dislodgementAndOccupation zonedResolvedOrders

    RetreatGame RetreatRoundOne _ turn _ zonedResolvedOrders occupation thisControl ->
        TypicalGame TypicalRoundTwo Unresolved turn nextZonedOrders thisControl
      where
        -- Give every occupier a hold order.
        nextZonedOrders :: M.Map Zone (Aligned Unit, SomeOrderObject Typical)
        nextZonedOrders = M.mapWithKey giveDefaultTypicalOrder nextOccupation

        giveDefaultTypicalOrder
            :: Zone
            -> Aligned Unit
            -> (Aligned Unit, SomeOrderObject Typical)
        giveDefaultTypicalOrder zone aunit = (aunit, SomeOrderObject object)
          where
            object = MoveObject (zoneProvinceTarget zone)

        -- Every dislodged unit which successfully withdraws is added to the
        -- next occupation value; all others are forgotten.
        nextOccupation :: Occupation
        nextOccupation = M.foldWithKey occupationFold occupation zonedResolvedOrders

        occupationFold
            :: Zone
            -> (Aligned Unit, SomeResolved OrderObject Retreat)
            -> Occupation
            -> Occupation
        occupationFold zone (aunit, SomeResolved (object, res)) =
            case (object, res) of
                (WithdrawObject withdrawingTo, Nothing) -> occupy withdrawingTo (Just aunit)
                _ -> id

    RetreatGame RetreatRoundTwo _ turn _ zonedResolvedOrders occupation thisControl ->
        AdjustGame AdjustRound Unresolved turn nextZonedOrders nextControl
      where
        nextZonedOrders :: M.Map Zone (Aligned Unit, SomeOrderObject Adjust)
        nextZonedOrders = M.mapWithKey giveDefaultAdjustOrder nextOccupation

        -- This one is not so trivial... what IS the default adjust order?
        -- It depends upon the defecit, and the distance of the unit from
        -- its home supply centre! That's because our goal is to enforce that
        -- the issued orders in a Game are always valid. So we can't just throw
        -- a bunch of Continue objects onto the order set here; the great power
        -- may need to disband some units!
        -- NB a player need not have a defecit of 0; it's ok to have a negative
        -- defecit, since the rule book states that a player may decline to
        -- build a unit that she is entitled to.
        --
        -- First, let's calculate the defecits for each great power.
        -- Then, we'll order their units by minimum distance from home supply
        -- centre.
        -- Then, we give as many disband orders as the defecit if it's positive,
        -- using the list order; other units get ContinueObject.
        --
        -- Associate every country with a list of the zones it occupies,
        -- ordered by distance from home supply centre.
        --
        -- TODO must respect the rule "in case of a tie, fleets first, then
        -- alphabetically by province".
        zonesByDistance :: M.Map GreatPower [Zone]
        zonesByDistance =
            M.mapWithKey
              (\k -> sortWith (distanceFromHomeSupplyCentre k . ptProvince . zoneProvinceTarget))
              (M.foldWithKey foldZonesByDistance M.empty occupation)

        sortWith f = sortBy (\x y -> f x `compare` f y)

        foldZonesByDistance
            :: Zone
            -> Aligned Unit
            -> M.Map GreatPower [Zone]
            -> M.Map GreatPower [Zone]
        foldZonesByDistance zone aunit = M.alter alteration (alignedGreatPower aunit)
          where
            alteration m = case m of
                Nothing -> Just [zone]
                Just zs -> Just (zone : zs)

        disbands :: S.Set Zone
        disbands = M.foldWithKey foldDisbands S.empty zonesByDistance

        foldDisbands
            :: GreatPower
            -> [Zone]
            -> S.Set Zone
            -> S.Set Zone
        -- take behaves as we want it to with negative numbers.
        foldDisbands greatPower zones = S.union (S.fromList (take defecit zones))
          where
            defecit = supplyCentreDefecit greatPower nextOccupation nextControl

        giveDefaultAdjustOrder
            :: Zone
            -> Aligned Unit
            -> (Aligned Unit, SomeOrderObject Adjust)
        giveDefaultAdjustOrder zone aunit = case S.member zone disbands of
            True -> (aunit, SomeOrderObject DisbandObject)
            False -> (aunit, SomeOrderObject ContinueObject)

        -- Every dislodged unit which successfully withdraws is added to the
        -- next occupation value; all others are forgotten.
        nextOccupation :: Occupation
        nextOccupation = M.foldWithKey occupationFold occupation zonedResolvedOrders

        occupationFold
            :: Zone
            -> (Aligned Unit, SomeResolved OrderObject Retreat)
            -> Occupation
            -> Occupation
        occupationFold zone (aunit, SomeResolved (object, res)) =
            case (object, res) of
                (WithdrawObject withdrawingTo, Nothing) -> occupy withdrawingTo (Just aunit)
                _ -> id

        -- Every unit in @nextOccupation@ takes control of the Province where it
        -- lies.
        nextControl :: Control
        nextControl = M.foldWithKey controlFold thisControl nextOccupation

        controlFold
            :: Zone
            -> Aligned Unit
            -> Control
            -> Control
        controlFold zone aunit = control (ptProvince (zoneProvinceTarget zone)) (Just (alignedGreatPower aunit))

    AdjustGame AdjustRound _ turn zonedResolvedOrders thisControl ->
        TypicalGame TypicalRoundOne Unresolved (nextTurn turn) nextZonedOrders thisControl
      where
        -- Give every occupier a hold order.
        nextZonedOrders :: M.Map Zone (Aligned Unit, SomeOrderObject Typical)
        nextZonedOrders = M.mapWithKey giveDefaultTypicalOrder nextOccupation

        giveDefaultTypicalOrder
            :: Zone
            -> Aligned Unit
            -> (Aligned Unit, SomeOrderObject Typical)
        giveDefaultTypicalOrder zone aunit = (aunit, SomeOrderObject object)
          where
            object = MoveObject (zoneProvinceTarget zone)

        -- Builds and continues become occupying units; disbands go away.
        nextOccupation :: Occupation
        nextOccupation = M.mapMaybe mapOccupation zonedResolvedOrders

        mapOccupation
            :: (Aligned Unit, SomeResolved OrderObject Adjust)
            -> Maybe (Aligned Unit)
        mapOccupation (aunit, SomeResolved (object, resolution)) =
            case (object, resolution) of
                (DisbandObject, Nothing) -> Nothing
                (BuildObject, Nothing) -> Just aunit
                (ContinueObject, Nothing) -> Just aunit
