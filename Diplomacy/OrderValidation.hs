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
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Diplomacy.OrderValidation (

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

  ) where

import Control.Applicative
import qualified Data.Map as M
import Data.MapUtil
import Data.AtLeast
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
    -- TODO uncomment this when we introduce convoy orders.
    -- && unitCannotConvoyFromTo unit from to
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
