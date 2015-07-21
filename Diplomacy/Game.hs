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
  , gameSupplyCentreDefecit
  , gameTurn
  , gameRound
  , gameSeason
  , issueOrder
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

gameControl :: Game round roundStatus -> Control
gameControl game = case game of
    TypicalGame _ _ _ _ c -> c
    RetreatGame _ _ _ _ _ _ c -> c
    AdjustGame _ _ _ _ c -> c

gameSupplyCentreDefecit
    :: GreatPower
    -> Game round roundStatus
    -> SupplyCentreDefecit
gameSupplyCentreDefecit power game = case game of
    AdjustGame _ Unresolved _ _ _ ->
        (M.size controlledUnits - M.size issuedDisbands + M.size issuedBuilds) - M.size controlledSupplyCentres
      where
        issuedDisbands = M.filter isIssuedDisband (gameZonedOrders game)
        issuedBuilds = M.filter isIssuedBuild (gameZonedOrders game)
        isIssuedDisband (aunit, SomeOrderObject order) = case order of
            DisbandObject -> alignedGreatPower aunit == power
            _ -> False
        isIssuedBuild (aunit, SomeOrderObject order) = case order of
            BuildObject -> alignedGreatPower aunit == power
            _ -> False
    _ -> M.size controlledUnits - M.size controlledSupplyCentres
  where
    controlledUnits = M.filter (\aunit -> alignedGreatPower aunit == power) (gameOccupation game)
    controlledSupplyCentres = M.filterWithKey (\p power' -> power == power' && supplyCentre p) (gameControl game)

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

issueOrder
    :: forall order round .
       Aligned (Order (RoundPhase round) order)
    -> Game round RoundUnresolved
    -> Either (SomeInvalidReason (RoundPhase round)) (Game round RoundUnresolved)
issueOrder aorder game = case validation of
    Nothing -> Right (issueOrderUnsafe aorder game)
    Just invalid -> Left (SomeInvalidReason invalid)
  where

    occupation = gameOccupation game

    order = alignedThing aorder
    greatPower = alignedGreatPower aorder

    validation :: Maybe (InvalidReason (RoundPhase round) order)
    validation = case orderObject order of
        MoveObject _ -> validateMove greatPower occupation order
        SupportObject _ _ -> validateSupport greatPower occupation order
        ConvoyObject _ _ -> validateConvoy greatPower occupation order
        SurrenderObject -> validateSurrender greatPower dislodged order
          where
            dislodged = gameDislodged game
        WithdrawObject _ -> validateWithdraw greatPower dislodged resolution order
          where
            dislodged = gameDislodged game
            resolution = case game of
                RetreatGame _ _ _ x _ _ _ -> x
        DisbandObject -> validateDisband greatPower occupation order
        BuildObject -> validateBuild greatPower defecit order
          where
            defecit = gameSupplyCentreDefecit greatPower game
        ContinueObject -> validateContinue greatPower occupation order

    issueOrderUnsafe
        :: forall round order .
           Aligned (Order (RoundPhase round) order)
        -> Game round RoundUnresolved
        -> Game round RoundUnresolved
    issueOrderUnsafe aorder game = case game of
        TypicalGame TypicalRoundOne s t zonedOrders v -> TypicalGame TypicalRoundOne s t (insertOrder zonedOrders) v
        TypicalGame TypicalRoundTwo s t zonedOrders v -> TypicalGame TypicalRoundTwo s t (insertOrder zonedOrders) v
        RetreatGame RetreatRoundOne s t res zonedOrders o c -> RetreatGame RetreatRoundOne s t res (insertOrder zonedOrders) o c
        RetreatGame RetreatRoundTwo s t res zonedOrders o c -> RetreatGame RetreatRoundTwo s t res (insertOrder zonedOrders) o c
        AdjustGame AdjustRound s t zonedOrders c -> AdjustGame AdjustRound s t (insertOrder zonedOrders) c
      where
        order = alignedThing aorder
        greatPower = alignedGreatPower aorder
        zone = Zone (subjectProvinceTarget (orderSubject order))
        unit = subjectUnit (orderSubject order)
        aunit = align unit greatPower
        object :: OrderObject (RoundPhase round) order
        object = orderObject order
        someObject :: SomeOrderObject (RoundPhase round)
        someObject = SomeOrderObject object
        insertOrder :: M.Map Zone (Aligned Unit, SomeOrderObject (RoundPhase round)) -> M.Map Zone (Aligned Unit, SomeOrderObject (RoundPhase round))
        insertOrder = M.alter (const (Just (aunit, someObject))) zone

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
        RetreatGame (nextRetreatRound round) Unresolved turn zonedResolvedOrders nextZonedOrders nextOccupation thisControl
      where
        -- Give every dislodged unit a surrender order.
        nextZonedOrders :: M.Map Zone (Aligned Unit, SomeOrderObject Retreat)
        nextZonedOrders = M.map giveDefaultRetreatOrder dislodged

        giveDefaultRetreatOrder
            :: Aligned Unit
            -> (Aligned Unit, SomeOrderObject Retreat)
        giveDefaultRetreatOrder aunit = (aunit, SomeOrderObject object)
          where
            object = SurrenderObject

        dislodged :: M.Map Zone (Aligned Unit)
        nextOccupation :: Occupation
        (dislodged, nextOccupation) = M.foldWithKey dislodgedAndOccupationFold (M.empty, M.empty) currentOccupation

        -- We use this to fold over the current occupation.
        -- At a given zone, we check whether a new unit occupies it (using
        -- typicalChange) and if so, we dislodge the unit at that zone unless
        -- it successfully moved away.
        dislodgedAndOccupationFold
            :: Zone
            -> Aligned Unit
            -> (M.Map Zone (Aligned Unit), Occupation)
            -> (M.Map Zone (Aligned Unit), Occupation)
        dislodgedAndOccupationFold zone aunit (d, o) =
            case (typicalChange zonedResolvedOrders zone, M.lookup zone zonedResolvedOrders) of
                -- No change here; this unit still occupies... unless it did
                -- a successful move.
                (Nothing, Just (_, SomeResolved (MoveObject pt, Nothing))) ->
                    (d, M.insert (Zone pt) aunit o)
                (Nothing, _) ->
                    (d, M.insert zone aunit o)
                -- Change here; the incoming unit now occupies, and the unit
                -- here is dislodged unless it moved away.
                (Just asubj, Just (_, SomeResolved (MoveObject pt, Nothing))) ->
                    (d, M.insert zone aunit' o)
                  where
                    aunit' = align (subjectUnit (alignedThing asubj)) (alignedGreatPower asubj)
                (Just asubj, _) ->
                    (M.insert zone aunit d, M.insert zone aunit' o)
                  where
                    aunit' = align (subjectUnit (alignedThing asubj)) (alignedGreatPower asubj)

        currentOccupation :: Occupation
        currentOccupation = M.map (\(a, _) -> a) zonedResolvedOrders

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
        foldDisbands power zones = S.union (S.fromList (take defecit zones))
          where
            defecit = gameSupplyCentreDefecit power game

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
