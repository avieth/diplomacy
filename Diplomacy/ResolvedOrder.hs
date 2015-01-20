{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types #-}

module Diplomacy.ResolvedOrder (

    OrderInvalid(..)

  , OrderValidation

  , ValidOrder
  , outValidOrder
  , SomeValidOrder(..)

  , makeSomeValidOrder
  , consumeSomeValidOrder

  , validOrderToAlignedUnit
  , validOrderToSubjectTarget

  , validOrderTypical
  , validOrderRetreat

  , validateOrderTypical
  , validateOrderRetreat
  , validateOrderAdjust

  , ResolvedOrder(..)

  , ValidOrders(..)
  , ResolvedOrders(..)

  , makeValidOrdersTypical
  , makeValidOrdersRetreat
  , makeValidOrdersAdjust

  , resolveOrdersTypical
  , resolveOrdersRetreat
  , resolveOrdersAdjust

  , validateBuild

  ) where

import qualified Data.Map as M
import           Control.Applicative
import           Control.Monad
import           Data.Maybe (mapMaybe)

import Diplomacy.Phase
import Diplomacy.Province
import Diplomacy.Unit
import Diplomacy.Order
import Diplomacy.Country
import Diplomacy.Order
import Diplomacy.Orders
import Diplomacy.Occupation
import Diplomacy.Dislodgement
import Diplomacy.Defecits

-- | Any order can be invalid. Each witness gives a reason.
--   The type parameter indicates the phase to which it is relevant.
data OrderInvalid phaseType orderType where

  -- | The subject of the order (unit and province target) is not consistent
  --   with the state of the board against which the order was issued.
  --   Applies for any phase.
  SubjectInvalid :: OrderInvalid phaseType orderType

  -- | Army cannot go in water, fleet cannot go inland, whether at the request
  --   of a move, withdraw, or build.
  --   Applies to all phases.
  UnitCannotGoHere :: OrderInvalid phaseType orderType

  -- | Unit cannot move from one ProvinceTarget to another ProvinceTarget.
  --   Applies only for the Typical phase type. There is another constructor
  --   for an invalid move in the retreat phase type.
  MoveTargetNotReachable :: OrderInvalid Typical Move

  -- | Supported unit is not there?
  SupportedUnitNotPresent :: OrderInvalid Typical Support

  SupporterNotAdjacent :: OrderInvalid Typical Support

  ArmyCannotConvoy :: OrderInvalid Typical Convoy

  CoastalFleetCannotConvoy :: OrderInvalid Typical Convoy

  -- | Convoy makes sense only for certain pairs of ProvinceTargets, those
  --   which are adjacent by a nonempty path through water provinces which
  --   is not a cycle!
  --
  --   Is it valid to convoy from one province to that same province?!
  --   Try to find this in the rule book!
  --   It should not be valid I think, because it would make rule resolution
  --   tricky (undefined?): suppose we have a successful convoy from x to x, and
  --   a move from y to x, both with support 1. Who wins? Yes, must rule out
  --   autoconvoy! We'll just put that into the NotConvoyAdjacent clause.
  NotConvoyAdjacent :: OrderInvalid Typical Convoy

  -- | A non-dislodged unit cannot surrender nor withdraw.
  UnitNotDislodged :: OrderInvalid Retreat orderType

  -- | A unit cannot withdraw into an attacking province.
  WithdrawIntoAttackingProvince :: OrderInvalid Retreat Withdraw

  -- | A unit cannot withdraw into a non-adjacent country.
  --   Distinct from NotReachable, which only arises in Typical phases.
  WithdrawTargetNotReachable :: OrderInvalid Retreat Withdraw

  -- | Cannot build if it would induce a supply centre defecit.
  --   UPDATE: this is removed! InsufficientSupplyCentres is in fact a cause
  --   for order FAILURE not INVALIDITY, in that it depends upon the other
  --   orders given! The player can disband other units, then build more, so we
  --   cannot count the supply defecit until the disband orders are present,
  --   because the number of valid disband orders must be subtracted from the
  --   defecit.
  --InsufficientSupplyCentres
  --  :: Country
  --  -> ProvinceTarget
  --  -> OrderInvalid Adjust

  -- | Can build only in home supply centres.
  --   Note that this covers two cases: when the subject picks out something
  --   which is not a supply centre, AND when it picks out something which IS
  --   a supply centre but is not a HOME supply centre of the order's issuing
  --   country.
  IsNotHomeSupplyCentre :: OrderInvalid Adjust Build

  -- Note that we do not include a constructor describing the case in which
  -- a Country attempts to disband and build on the same supply centres. That's
  -- because this behaviour is rules out by the nature of the Board datatype,
  -- which associates at most one ValidOrder with each ProvinceTarget.

instance Show (OrderInvalid phaseType orderType) where
  show (SubjectInvalid) = "Subject invalid"
  show (UnitCannotGoHere) = "Unit cannot go here"
  show (MoveTargetNotReachable) = "Not reachable move"
  show (SupportedUnitNotPresent) = "Supported unit not present"
  show (SupporterNotAdjacent) = "Supporter not adjacent to target"
  show (ArmyCannotConvoy) = "Army cannot convoy"
  show (CoastalFleetCannotConvoy) = "Coastal fleet cannot convoy"
  show (NotConvoyAdjacent) = "No convoy path"
  show (UnitNotDislodged) = "Unit is not dislodged"
  show (WithdrawIntoAttackingProvince) = "Withdraw into attacking province"
  show (WithdrawTargetNotReachable) = "Withdraw target not reachable"
  show (IsNotHomeSupplyCentre) = "Not home supply centre"

-- | A newtype to distinguish Orders which have been validated.
--   Would be cool if we could associate them with a Board in the type, but
--   that would demand some dependent type action.
--
--   There are three ways in which ValidOrder values come about:
--
--     1) Default ValidOrders for Typical and Retreat phases, which depend upon
--        a Country, Unit, and ProvinceTarget. These are
--          validOrderTypical
--          validOrderRetreat
--     2) Pass an Order through validateOrder against a particular Board.
--        This will produce a ValidOrder only if the Order is syntactically
--        appropriate, given the state of the Board. An order to move some
--        Country's Unit from some ProvinceTarget which it does not actually
--        occupy, for instance, would not result in a ValidOrder, but an
--        OrderInvalid value (see OrderValidation type).
--
newtype ValidOrder phaseType orderType = ValidOrder {
    validOrder :: Order phaseType orderType
  } deriving (Show, Eq)

outValidOrder :: ValidOrder phaseType orderType -> Order phaseType orderType
outValidOrder = validOrder

validOrderToAlignedUnit :: ValidOrder phaseType orderType -> AlignedUnit
validOrderToAlignedUnit validOrder = align unit country
  where
    unit = orderSubjectUnit . orderSubject . outValidOrder $ validOrder
    country = orderCountry . outValidOrder $ validOrder

validOrderToSubjectTarget :: ValidOrder phaseType orderType -> ProvinceTarget
validOrderToSubjectTarget = orderSubjectTarget . orderSubject . outValidOrder

-- | A Board Typical or Board Retreat needs to associate any old ValidOrder for
--   a particlar phaseType with each occupied ProvinceTarget. It can't say
--   anything about the orderType. That must be recovered by pattern matching
--   on a SomeValidOrder, as we define here:
data SomeValidOrder phaseType where

  ValidHold :: ValidOrder Typical Hold -> SomeValidOrder Typical
  ValidMove :: ValidOrder Typical Move -> SomeValidOrder Typical
  ValidSupport :: ValidOrder Typical Support -> SomeValidOrder Typical
  ValidConvoy :: ValidOrder Typical Convoy -> SomeValidOrder Typical

  ValidSurrender :: ValidOrder Retreat Surrender -> SomeValidOrder Retreat
  ValidWithdraw :: ValidOrder Retreat Withdraw -> SomeValidOrder Retreat

  ValidDisband :: ValidOrder Adjust Disband -> SomeValidOrder Adjust
  ValidBuild :: ValidOrder Adjust Build -> SomeValidOrder Adjust 

makeSomeValidOrder :: ValidOrder phaseType orderType -> SomeValidOrder phaseType
makeSomeValidOrder vord = case outValidOrder vord of
  h@(HoldOrder _ _ _) -> ValidHold vord
  m@(MoveOrder _ _ _) -> ValidMove vord
  s@(SupportOrder _ _ _) -> ValidSupport vord
  c@(ConvoyOrder _ _ _) -> ValidConvoy vord
  s@(SurrenderOrder _ _ _) -> ValidSurrender vord
  w@(WithdrawOrder _ _ _) -> ValidWithdraw vord
  d@(DisbandOrder _ _ _) -> ValidDisband vord
  b@(BuildOrder _ _ _) -> ValidBuild vord

consumeSomeValidOrder
  :: (forall orderType . ValidOrder phaseType orderType -> a)
  -> SomeValidOrder phaseType
  -> a
consumeSomeValidOrder f someValidOrder = case someValidOrder of
  ValidHold v -> f v
  ValidMove v -> f v
  ValidSupport v -> f v
  ValidConvoy v -> f v
  ValidSurrender v -> f v
  ValidWithdraw v -> f v
  ValidDisband v -> f v
  ValidBuild v -> f v

isValidHold :: SomeValidOrder phaseType -> Maybe (ValidOrder Typical Hold)
isValidHold (ValidHold v) = Just v
isValidHold _ = Nothing

isValidMove :: SomeValidOrder phaseType -> Maybe (ValidOrder Typical Move)
isValidMove (ValidMove v) = Just v
isValidMove _ = Nothing

isValidSupport :: SomeValidOrder phaseType -> Maybe (ValidOrder Typical Support)
isValidSupport (ValidSupport v) = Just v
isValidSupport _ = Nothing

isValidConvoy :: SomeValidOrder phaseType -> Maybe (ValidOrder Typical Convoy)
isValidConvoy (ValidConvoy v) = Just v
isValidConvoy _ = Nothing

isValidSurrender :: SomeValidOrder phaseType -> Maybe (ValidOrder Retreat Surrender)
isValidSurrender (ValidSurrender v) = Just v
isValidSurrender _ = Nothing

isValidWithdraw :: SomeValidOrder phaseType -> Maybe (ValidOrder Retreat Withdraw)
isValidWithdraw (ValidWithdraw v) = Just v
isValidWithdraw _ = Nothing

isValidDisband :: SomeValidOrder phaseType -> Maybe (ValidOrder Adjust Disband)
isValidDisband (ValidDisband v) = Just v
isValidDisband _ = Nothing

isValidBuild :: SomeValidOrder phaseType -> Maybe (ValidOrder Adjust Build)
isValidBuild (ValidBuild v) = Just v
isValidBuild _ = Nothing

-- | Due to the disparity of the orderType parameter, we can't just throw all
--  ValidOrders into one list. Instead, we package them together according to
--  phaseType.
data ValidOrders phaseType where

  ValidOrdersTypical
    :: [SomeValidOrder Typical]
    -> ValidOrders Typical

  ValidOrdersRetreat
    :: [SomeValidOrder Retreat]
    -> ValidOrders Retreat

  ValidOrdersAdjust
    :: [SomeValidOrder Adjust]
    -> ValidOrders Adjust

makeValidOrdersTypical :: [SomeValidOrder Typical] -> ValidOrders Typical
makeValidOrdersTypical = ValidOrdersTypical

makeValidOrdersRetreat :: [SomeValidOrder Retreat] -> ValidOrders Retreat
makeValidOrdersRetreat = ValidOrdersRetreat

makeValidOrdersAdjust :: [SomeValidOrder Adjust] -> ValidOrders Adjust
makeValidOrdersAdjust = ValidOrdersAdjust

validHoldOrders
  :: ValidOrders Typical
  -> [ValidOrder Typical Hold]
validHoldOrders (ValidOrdersTypical someValidOrders) = mapMaybe isValidHold someValidOrders

validMoveOrders
  :: ValidOrders Typical
  -> [ValidOrder Typical Move]
validMoveOrders (ValidOrdersTypical someValidOrders) = mapMaybe isValidMove someValidOrders

validSupportOrders
  :: ValidOrders Typical
  -> [ValidOrder Typical Support]
validSupportOrders (ValidOrdersTypical someValidOrders) = mapMaybe isValidSupport someValidOrders

validConvoyOrders
  :: ValidOrders Typical
  -> [ValidOrder Typical Convoy]
validConvoyOrders (ValidOrdersTypical someValidOrders) = mapMaybe isValidConvoy someValidOrders

validSurrenderOrders
  :: ValidOrders Retreat
  -> [ValidOrder Retreat Surrender]
validSurrenderOrders (ValidOrdersRetreat someValidOrders) = mapMaybe isValidSurrender someValidOrders

validWithdrawOrders
  :: ValidOrders Retreat
  -> [ValidOrder Retreat Withdraw]
validWithdrawOrders (ValidOrdersRetreat someValidOrders) = mapMaybe isValidWithdraw someValidOrders

validDisbandOrders
  :: ValidOrders Adjust
  -> [ValidOrder Adjust Disband]
validDisbandOrders (ValidOrdersAdjust someValidOrders) = mapMaybe isValidDisband someValidOrders

validBuildOrders
  :: ValidOrders Adjust
  -> [ValidOrder Adjust Build]
validBuildOrders (ValidOrdersAdjust someValidOrders) = mapMaybe isValidBuild someValidOrders

type OrderValidation phaseType orderType
  = Either (OrderInvalid phaseType orderType) (ValidOrder phaseType orderType)

type Validation phaseType orderType = Maybe (OrderInvalid phaseType orderType)

orderValidation
  :: Order phaseType orderType
  -> Validation phaseType orderType
  -> OrderValidation phaseType orderType
orderValidation _ (Just x) = Left $ x
orderValidation x Nothing = Right $ ValidOrder x

ok :: Validation phaseType orderType
ok = Nothing

notOk :: OrderInvalid phaseType orderType -> Validation phaseType orderType
notOk = Just

type Validator phaseType orderType
  = Occupation AlignedUnit -> Order phaseType orderType -> Validation phaseType orderType

also
  :: Validator phaseType orderType
  -> Validator phaseType orderType
  -> Validator phaseType orderType
also check1 check2 board order = check1 board order <|> check2 board order

orElse :: Bool -> OrderInvalid phaseType orderType -> Validation phaseType orderType
orElse True _ = ok
orElse False x = notOk x

implies :: Bool -> OrderInvalid phaseType orderType -> Validation phaseType orderType
implies True x = notOk x
implies False _ = ok

-- | Build a ValidOrder Typical without reference to a Board.
--   Use with care! The intention is to facilitate "boostrapping" a Board
--   with the default ValidOrder values, and once they are present, the Board
--   can be used with validateOrder to produce new ValidOrder via
--   OrderValidation.
validOrderTypical :: Country -> ProvinceTarget -> Unit -> ValidOrder Typical Hold
validOrderTypical country provinceTarget unit = ValidOrder order
  where
    subject = OrderSubject unit provinceTarget
    order = makeOrder country subject defaultOrderObjectTypical

-- | See explanation of validOrderTypical. Same idea.
validOrderRetreat :: Country -> ProvinceTarget -> Unit -> ValidOrder Retreat Surrender
validOrderRetreat country provinceTarget unit = ValidOrder order
  where
    subject = OrderSubject unit provinceTarget
    order = makeOrder country subject defaultOrderObjectRetreat

validateOrderTypical
  :: Occupation AlignedUnit
  -> Order Typical orderType
  -> OrderValidation Typical orderType
validateOrderTypical occupation order = orderValidation order validation
  where
    validation = (validateSubjectTypical occupation order) <|> (validateObjectTypical occupation order)

validateOrderRetreat
  :: Dislodgement AlignedUnit
  -> ResolvedOrders Typical
  -> Order Retreat orderType
  -> OrderValidation Retreat orderType
validateOrderRetreat dislodgement resolutions order = orderValidation order validation
  where
    validation = (validateSubjectRetreat dislodgement order) <|> (validateObjectRetreat resolutions order)

validateOrderAdjust
  :: Occupation AlignedUnit
  -> Order Adjust orderType
  -> OrderValidation Adjust orderType
validateOrderAdjust occupation order = orderValidation order validation
  where
    validation = (validateSubjectAdjust occupation order) <|> (validateObjectAdjust order)

validateSubjectTypical
  :: Occupation AlignedUnit
  -> Order Typical orderType
  -> Validation Typical orderType
validateSubjectTypical occupation order = condition `orElse` reason
  where
    condition = occupies au provinceTarget occupation
    reason = SubjectInvalid
    country = orderCountry order
    subject = orderSubject order
    au = align unit country
    unit = orderSubjectUnit subject
    provinceTarget = orderSubjectTarget subject

validateSubjectRetreat
  :: Dislodgement AlignedUnit
  -> Order Retreat orderType
  -> Validation Retreat orderType
validateSubjectRetreat dislodgement order = condition `orElse` reason
  where
    condition = dislodged au provinceTarget dislodgement
    reason = SubjectInvalid
    country = orderCountry order
    subject = orderSubject order
    au = align unit country
    unit = orderSubjectUnit subject
    provinceTarget = orderSubjectTarget subject

validateSubjectAdjust
  :: Occupation AlignedUnit
  -> Order Adjust orderType
  -> Validation Adjust orderType
validateSubjectAdjust occupation order = case order of
    DisbandOrder country subject _ -> 
      let condition = occupies au provinceTarget occupation
          reason = SubjectInvalid
          au = align unit country
          unit = orderSubjectUnit subject
          provinceTarget = orderSubjectTarget subject
      in  condition `orElse` reason
    -- Build order subject is always OK; we check for home supply centre as
    -- a part of the object validity.
    BuildOrder _ _ _ -> ok

validateObjectTypical
  :: Occupation AlignedUnit
  -> Order Typical orderType
  -> Validation Typical orderType
validateObjectTypical occupation order = case order of
  HoldOrder country subject object -> validateHold
  MoveOrder country subject object -> validateMove order
  SupportOrder country subject object -> validateSupport occupation order
  ConvoyOrder country subject object -> validateConvoy order

validateObjectRetreat
  :: ResolvedOrders Typical
  -> Order Retreat orderType
  -> Validation Retreat orderType
validateObjectRetreat resolutions order = case order of
  SurrenderOrder country subject object -> validateSurrender
  WithdrawOrder country subject object -> validateWithdraw resolutions order

validateObjectAdjust
  :: Order Adjust orderType
  -> Validation Adjust orderType
validateObjectAdjust order = case order of
  DisbandOrder country subject object -> validateDisband
  BuildOrder country subject object -> validateBuild order

validateHold :: Validation Typical Hold
validateHold = ok

validateMove :: Order Typical Move -> Validation Typical Move
validateMove (MoveOrder country subject (Move provinceTarget)) =
        unitCannotOccupy unit provinceTarget
    <|> cannotMoveFromTo unit fromPt provinceTarget
  where
    unit = orderSubjectUnit subject
    fromPt = orderSubjectTarget subject

validateSupport
  :: Occupation AlignedUnit
  -> Order Typical Support
  -> Validation Typical Support
validateSupport occupation (SupportOrder country subject (Support unit ptFrom ptInto)) =
        supportedUnitPresent `orElse` SupportedUnitNotPresent
    <|> unitCannotOccupy supportingUnit ptInto
    <|> couldNotAttack `orElse` SupporterNotAdjacent
  where
    supportingUnit = orderSubjectUnit subject
    supportingFrom = orderSubjectTarget subject
    couldNotAttack = elem ptInto (neighbours supportingFrom)
    supportedUnitPresent = maybe False ((==) unit . alignedUnit) (checkOccupation ptFrom occupation)

validateConvoy :: Order Typical Convoy -> Validation Typical Convoy
validateConvoy (ConvoyOrder country subject (Convoy unit ptFrom ptTo)) =
        isFleet unit `orElse` ArmyCannotConvoy
    <|> isWater pSubject `orElse` CoastalFleetCannotConvoy
    <|> elem ptTo validConvoyTargets `orElse` NotConvoyAdjacent
  where
    ptSource = orderSubjectTarget subject
    pSubject = ptProvince (orderSubjectTarget subject)
    validConvoyTargets = filter (/= ptFrom) (waterReachables ptFrom)

-- A surrender always passes validation so long as its subject is assumed to
-- be valid.
validateSurrender :: Validation Retreat Surrender
validateSurrender = ok

validateWithdraw
  :: ResolvedOrders Typical
  -> Order Retreat Withdraw
  -> Validation Retreat Withdraw
validateWithdraw resolutions order@(WithdrawOrder country subject (Withdraw toProvinceTarget)) =
        unitCannotOccupy unit toProvinceTarget
    <|> notWithdrawAdjacent subject toProvinceTarget
    <|> withdrawingIntoAttackingProvince resolutions order
  where
    unit = orderSubjectUnit subject

-- | Disband is OK so long as its subject is OK.
validateDisband :: Validation Adjust Disband
validateDisband = ok

validateBuild :: Order Adjust Build -> Validation Adjust Build
validateBuild (BuildOrder country subject _) =
        unitCannotOccupy unit provinceTarget
    <|> notInHomeSupplyCentre country provinceTarget
  where
    unit = orderSubjectUnit subject
    provinceTarget = orderSubjectTarget subject

notInHomeSupplyCentre :: Country -> ProvinceTarget -> Validation Adjust Build
notInHomeSupplyCentre country provinceTarget = condition `orElse` reason
  where
    province = ptProvince provinceTarget
    isSupplyCentre = supplyCentre province
    isHomeProvince = isHome country province
    condition = isHomeProvince && isSupplyCentre
    reason = IsNotHomeSupplyCentre

unitCannotOccupy :: Unit -> ProvinceTarget -> Validation phaseType orderType
unitCannotOccupy u pt = condition `implies` reason
  where
    condition =  (isArmy u && isWater (ptProvince pt))
              || (isFleet u && isInland (ptProvince pt))
              || (isArmy u && isSpecial pt)
              || (isFleet u && not (null (provinceCoasts (ptProvince pt))) && not (isSpecial pt))
              -- This last clause ensures that a fleet cannot go on a Normal part
              -- of a ProvinceTarget which has special coastlines.
    reason = UnitCannotGoHere

cannotMoveFromTo :: Unit -> ProvinceTarget -> ProvinceTarget -> Validation Typical Move
cannotMoveFromTo unit fromPt toPt = condition `orElse` reason
  where
    reason = MoveTargetNotReachable
    reachableByConvoy = filter (/= fromPt) (waterReachables fromPt)
    reachableOtherwise = neighbours fromPt
    sharedCoastline = commonCoasts fromPt toPt
    condition = if isArmy unit
                then elem toPt reachableOtherwise || elem toPt reachableByConvoy
                -- Fleet Movement section of the wizards.com rules
                -- say that a fleet can move from coastal province to coastal
                -- province iff they share a coastline.
                -- We know already that ptProvince fromPt is not inland, else
                -- invalidity would have been established earlier, because no
                -- fleet can be in an inland province.
                else if isCoastal (ptProvince fromPt) && isCoastal (ptProvince toPt)
                     then elem toPt reachableOtherwise && not (null sharedCoastline)
                     else elem toPt reachableOtherwise

-- Unlike move adjacency, here we do not admit convoy adjacency.
notWithdrawAdjacent :: OrderSubject -> ProvinceTarget -> Validation Retreat Withdraw
notWithdrawAdjacent subject toPt = condition `orElse` reason
  where
    unit = orderSubjectUnit subject
    fromPt = orderSubjectTarget subject
    reason = WithdrawTargetNotReachable
    condition = elem toPt (neighbours fromPt)

withdrawingIntoAttackingProvince
  :: ResolvedOrders Typical
  -> Order Retreat Withdraw
  -> Validation Retreat Withdraw
withdrawingIntoAttackingProvince resolutions order = condition `implies` reason
  where
    condition = not (null dislodgingMoves)
    reason = WithdrawIntoAttackingProvince
    withdrawingFrom = orderSubjectTarget (orderSubject order)
    dislodgingMoves = withMoves resolutions selector
    selector (validMove, resolvedMove) = case resolvedMove of
      Failed _ -> Nothing
      Succeeded _ -> case outValidOrder validMove of
        MoveOrder _ _ (Move toPt) ->
          if toPt == withdrawingFrom
          then Just ()
          else Nothing

--                      --
-- * ORDER RESOLUTION * --
--                      --

data ResolvedOrder phaseType orderType
  = Succeeded (OrderSucceeded phaseType orderType)
  | Failed (OrderFailed phaseType orderType)

-- Similar to Validation.
type Resolution phaseType orderType = Maybe (OrderFailed phaseType orderType)

-- Similar to orderValidation
orderResolution
  :: Resolution phaseType orderType
  -> ResolvedOrder phaseType orderType
orderResolution (Just x) = Failed x
orderResolution Nothing = Succeeded OrderSucceeded

data OrderSucceeded phaseType orderType where
  OrderSucceeded :: OrderSucceeded phaseType orderType

-- | Reasons why an Order could fail.
data OrderFailed phaseType orderType where

  HoldOverpowered
    :: ValidOrder Typical Move
    -- ^ The move which overpowered the hold.
    -> OrderFailed Typical Hold

  MoveOverpowered
    :: ValidOrder Typical Move
    -> OrderFailed Typical Move

  -- | Failed to dislodge a hold.
  Defended
    :: ValidOrder Typical Hold
    -> OrderFailed Typical Move

  Standoff
    :: [ValidOrder Typical Move]
    -> OrderFailed Typical Move

  -- | A move which needed a convoy did not get a convoy.
  NoConvoy
    :: OrderFailed Typical Move

  WouldDislodgeOwnUnit
    :: OrderFailed Typical Move

  SupportCut
    :: [ValidOrder Typical Move]
    -> OrderFailed Typical Support

  SupportDislodged
    :: ValidOrder Typical Move
    -> OrderFailed Typical Support

  ConvoyDislodged
    :: ValidOrder Typical Move
    -> OrderFailed Typical Convoy

  -- Failed because either the expected move order wasn't placed, or because
  -- other expected convoy orders were not placed.
  ConvoyFailed
    :: OrderFailed Typical Convoy

  -- | The expected move order wasn't made.
  ConvoyDeclined 
    :: OrderFailed Typical Convoy

  WithdrawConflict
    :: [ValidOrder Retreat Withdraw]
    -> OrderFailed Retreat Withdraw

  InsufficientSupplyCentres
    :: OrderFailed Adjust Build

-- | So we have this datatype, which we aim to compute from a set of
--   ValidOrder phaseType.
data ResolvedOrders phaseType where

  ResolvedOrdersTypical
    -- Unfortunately we have to have a different list for each order type.
    -- I'm sure there is a way around it; we can look into this later.
    -- Also, consider using Set? Worth it?
    :: [(ValidOrder Typical Hold, ResolvedOrder Typical Hold)]
    -> [(ValidOrder Typical Move, ResolvedOrder Typical Move)]
    -> [(ValidOrder Typical Support, ResolvedOrder Typical Support)]
    -> [(ValidOrder Typical Convoy, ResolvedOrder Typical Convoy)]
    -> ResolvedOrders Typical

  ResolvedOrdersRetreat
    :: [(ValidOrder Retreat Surrender, ResolvedOrder Retreat Surrender)]
    -> [(ValidOrder Retreat Withdraw, ResolvedOrder Retreat Withdraw)]
    -> ResolvedOrders Retreat

  ResolvedOrdersAdjust
    :: [(ValidOrder Adjust Disband, ResolvedOrder Adjust Disband)]
    -> [(ValidOrder Adjust Build, ResolvedOrder Adjust Build)]
    -> ResolvedOrders Adjust

-- | The resolution of a ValidOrder phaseType depends only upon a
--   ResolutionContext phaseType.
data ResolutionContext phaseType where

  ResolutionContextTypical
    :: ResolvedOrders Typical
    -> ResolutionContext Typical

  ResolutionContextRetreat
    :: ResolvedOrders Retreat
    -> ResolutionContext Retreat

  ResolutionContextAdjust
    :: ResolvedOrders Adjust
    -> Defecits
    -> ResolutionContext Adjust

extractResolution :: ResolutionContext phaseType -> ResolvedOrders phaseType
extractResolution (ResolutionContextTypical r) = r
extractResolution (ResolutionContextRetreat r) = r
extractResolution (ResolutionContextAdjust r _) = r

fixedDefecit
  :: ResolutionContext Adjust
  -> Country
  -> Int
fixedDefecit (ResolutionContextAdjust _ defecits) country = defecits country

withHolds
  :: ResolvedOrders Typical
  -> ((ValidOrder Typical Hold, ResolvedOrder Typical Hold) -> Maybe a)
  -> [a]
withHolds (ResolvedOrdersTypical hs _ _ _) f = mapMaybe f hs

withMoves
  :: ResolvedOrders Typical
  -> ((ValidOrder Typical Move, ResolvedOrder Typical Move) -> Maybe a)
  -> [a]
withMoves (ResolvedOrdersTypical _ ms _ _) f = mapMaybe f ms

withSupports
  :: ResolvedOrders Typical
  -> ((ValidOrder Typical Support, ResolvedOrder Typical Support) -> Maybe a)
  -> [a]
withSupports (ResolvedOrdersTypical _ _ ss _) f = mapMaybe f ss

withConvoys
  :: ResolvedOrders Typical
  -> ((ValidOrder Typical Convoy, ResolvedOrder Typical Convoy) -> Maybe a)
  -> [a]
withConvoys (ResolvedOrdersTypical _ _ _ cs) f = mapMaybe f cs

withWithdraws
  :: ResolvedOrders Retreat
  -> ((ValidOrder Retreat Withdraw, ResolvedOrder Retreat Withdraw) -> Maybe a)
  -> [a]
withWithdraws (ResolvedOrdersRetreat _ ws) f = mapMaybe f ws

withDisbands
  :: ResolvedOrders Adjust
  -> ((ValidOrder Adjust Disband, ResolvedOrder Adjust Disband) -> Maybe a)
  -> [a]
withDisbands (ResolvedOrdersAdjust ds _) f = mapMaybe f ds

withBuilds
  :: ResolvedOrders Adjust
  -> ((ValidOrder Adjust Build, ResolvedOrder Adjust Build) -> Maybe a)
  -> [a]
withBuilds (ResolvedOrdersAdjust _ bs) f = mapMaybe f bs

-- | Count the number of successful supports for a given move.
countSupportMove
  :: ResolutionContext Typical
  -> ValidOrder Typical Move
  -> Int
countSupportMove ctx vmove = length successfulSupports

  where

    successfulSupports :: [OrderSucceeded Typical Support]
    successfulSupports = withSupports (extractResolution ctx) (\(vsupp, res) ->
          let SupportOrder _ _ (Support _ supportingFrom supportingTo) = outValidOrder vsupp
          in  if movingFrom == supportingFrom && movingTo == supportingTo
              then case res of
                Succeeded s -> Just s
                Failed _ -> Nothing
              else Nothing
        )

    MoveOrder _ subject (Move movingTo) = outValidOrder vmove

    movingFrom = orderSubjectTarget subject

-- | Count the number of successful supports for a given hold.
--   Much in common with countSupportMove. Potential factoring via
--     countSupport
--       :: ResolutionContext Typical
--       -> ProvinceTarget
--       -> ProvinceTarget
--       -> Int
countSupportHold
  :: ResolutionContext Typical
  -> ValidOrder Typical Hold
  -> Int
countSupportHold ctx vhold = length successfulSupports

  where

    successfulSupports :: [OrderSucceeded Typical Support]
    successfulSupports = withSupports (extractResolution ctx) (\(vsupp, res) ->
          let SupportOrder _ _ (Support _ supportingFrom supportingTo) = outValidOrder vsupp
          in  if supportingFrom == holdingAt && supportingTo == holdingAt
              then case res of
                Succeeded s -> Just s
                Failed _ -> Nothing
              else Nothing
        )

    HoldOrder _ subject _ = outValidOrder vhold

    holdingAt = orderSubjectTarget subject

movesWithTarget
  :: ResolutionContext Typical
  -> ProvinceTarget
  -> [(ValidOrder Typical Move, ResolvedOrder Typical Move)]
movesWithTarget ctx provinceTarget = withMoves (extractResolution ctx) selector
  where
    selector (vmove, res) =
      let MoveObject m = orderObject (outValidOrder vmove)
          movingTo = moveTarget m
      in  if movingTo == provinceTarget
          then Just (vmove, res)
          else Nothing

-- | The ValidOrder Retreat Withdraw values in the ResolutionContext which
--   have the same target as a given ValidOrder Retreat Withdraw. Will not
--   include the input ValidOrder.
conflictingWithdraws
  :: ResolutionContext Retreat
  -> ValidOrder Retreat Withdraw
  -> [ValidOrder Retreat Withdraw]
conflictingWithdraws ctx validWithdraw = conflicting
  where
    conflicting :: [ValidOrder Retreat Withdraw]
    conflicting = withWithdraws (extractResolution ctx) (\(vwith, _) ->
          let WithdrawOrder _ _ (Withdraw intoProvince') = outValidOrder vwith
          in  if vwith /= validWithdraw && intoProvince == intoProvince'
              then Just vwith
              else Nothing
        )

    WithdrawOrder _ _ (Withdraw intoProvince) = outValidOrder validWithdraw

resolveOrdersTypical :: ValidOrders Typical -> ResolvedOrders Typical
resolveOrdersTypical vords =
    let (ctx, res) = (ResolutionContextTypical res, ResolvedOrdersTypical (holds ctx) (moves ctx) (supports ctx) (convoys ctx))
    in  extractResolution ctx -- = res
  where
    holds ctx = map (\vord -> (vord, resolveHold ctx vord)) (validHoldOrders vords)
    moves ctx = map (\vord -> (vord, resolveMove ctx vord)) (validMoveOrders vords)
    supports ctx = map (\vord -> (vord, resolveSupport ctx vord)) (validSupportOrders vords)
    convoys ctx = map (\vord -> (vord, resolveConvoy ctx vord)) (validConvoyOrders vords)

resolveOrdersRetreat :: ValidOrders Retreat -> ResolvedOrders Retreat
resolveOrdersRetreat vords =
    let (ctx, res) = (ResolutionContextRetreat res, ResolvedOrdersRetreat (surrenders ctx) (withdraws ctx))
    in  extractResolution ctx -- = res
  where
    surrenders ctx = map (\vord -> (vord, resolveSurrender ctx vord)) (validSurrenderOrders vords)
    withdraws ctx = map (\vord -> (vord, resolveWithdraw ctx vord)) (validWithdrawOrders vords)

resolveOrdersAdjust :: Defecits -> ValidOrders Adjust -> ResolvedOrders Adjust
resolveOrdersAdjust defecits vords =
    let (ctx, res) = (ResolutionContextAdjust res defecits, ResolvedOrdersAdjust (disbands ctx) (builds ctx))
    in  extractResolution ctx -- = res
  where
    disbands ctx = map (\vord -> (vord, resolveDisband ctx vord)) (validDisbandOrders vords)
    builds ctx = map (\vord -> (vord, resolveBuild ctx vord)) (validBuildOrders vords)

-- | Resolve a hold order.
--
--   A hold fails if and only if there is some move into its territory which
--   has strictly more support than it and all other moves against it.
--   We can't appeal to the resolutions of the move orders into the holding
--   target, because those may depend upon this resolution!
resolveHold
  :: ResolutionContext Typical
  -> ValidOrder Typical Hold
  -> ResolvedOrder Typical Hold
resolveHold ctx validHoldOrder = orderResolution holdOverpowered

  where

    holdOverpowered :: Resolution Typical Hold
    holdOverpowered = case mostMoveSupport of
      -- There's no move with strictly the most support, so this hold must
      -- succeed (if there are moves, they'll be a standoff).
      Nothing -> Nothing
      -- There's a dominating move; must check that it has strictly more
      -- support than this hold does.
      Just ((validMove, res), moveSupport) ->
        if moveSupport > holdSupport
        -- Are we safe to force res in case the condition passes?
        -- Do we need to? Can't we just fake it and drop in OrderSucceeded?
        then Just (HoldOverpowered validMove)
        else Nothing

    holdSupport :: Int
    holdSupport = countSupportHold ctx validHoldOrder

    mostMoveSupport :: Maybe ((ValidOrder Typical Move, ResolvedOrder Typical Move), Int)
    mostMoveSupport = strictMaximum (countSupportMove ctx . fst) attackingMoves

    attackingMoves :: [(ValidOrder Typical Move, ResolvedOrder Typical Move)]
    attackingMoves = withMoves (extractResolution ctx) (\(vmove, res) ->
          let MoveOrder _ _ (Move movingTo) = outValidOrder vmove
          in  if movingTo == holdingAt
              then Just (vmove, res)
              else Nothing
        )

    holdingAt :: ProvinceTarget
    holdingAt = orderSubjectTarget (orderSubject (outValidOrder validHoldOrder))

-- This will be the hardest one I think.
resolveMove
  :: ResolutionContext Typical
  -> ValidOrder Typical Move
  -> ResolvedOrder Typical Move
resolveMove ctx validMoveOrder =
    if requiresConvoy validMoveOrder
    then resolveConvoyedMove ctx validMoveOrder
    else resolveNormalMove ctx validMoveOrder
  where
    requiresConvoy :: ValidOrder Typical Move -> Bool
    requiresConvoy validMove =
        let MoveOrder _ subject (Move movingTo) = outValidOrder validMove
            movingFrom = orderSubjectTarget subject
        in  elem movingTo (neighbours movingFrom)

-- | First, establish that the convoy route succeeds.
--   If so, fall back to normal move resolution.
resolveConvoyedMove
  :: ResolutionContext Typical
  -> ValidOrder Typical Move
  -> ResolvedOrder Typical Move
resolveConvoyedMove ctx validMoveOrder = orderResolution $
    (noConvoyForMove ctx validMoveOrder) <|> (moveOverpowered ctx validMoveOrder)

resolveNormalMove
  :: ResolutionContext Typical
  -> ValidOrder Typical Move
  -> ResolvedOrder Typical Move
resolveNormalMove ctx validMoveOrder = orderResolution $
    moveOverpowered ctx validMoveOrder

noConvoyForMove
  :: ResolutionContext Typical
  -> ValidOrder Typical Move
  -> Resolution Typical Move
noConvoyForMove ctx validMove = case successfulConvoyRoutes of
    [] -> Just NoConvoy
    _ -> Nothing

  where

    -- We shall examine all successful convoy orders which reference this move
    -- order, and separate them into paths from source to target. If there is
    -- at least one path, then it's all good, the move can convoy.
    --
    -- Note that paradoxes like that of Pandin are taken care of not here, but
    -- in resolveSupport and resolveConvoy. A convoy will fail if any
    -- attacker has strictly more support than it does, regardless of whether
    -- there would be a standoff, so if we get a complete convoy route here
    -- then we know that this move order could not cut support that was
    -- essential for a dislodgement of one of the convoying fleets, because
    -- any such support we know already to be insufficient!

    successfulConvoyRoutes :: [[ValidOrder Typical Convoy]]
    successfulConvoyRoutes = map fst $ steps initialConvoys adjacentConvoys isTerminalConvoy

    isTerminalConvoy :: ValidOrder Typical Convoy -> Bool
    isTerminalConvoy validOrder =
        let ConvoyOrder _ subject _ = outValidOrder validOrder
            convoyingAt = orderSubjectTarget subject
        in  elem movingTo (neighbours convoyingAt)

    adjacentConvoys :: ValidOrder Typical Convoy -> [ValidOrder Typical Convoy]
    adjacentConvoys vconvoy = filter (isAdjacentConvoy vconvoy) convoys

    isAdjacentConvoy :: ValidOrder Typical Convoy -> ValidOrder Typical Convoy -> Bool
    isAdjacentConvoy vconvoy1 vconvoy2 =
        let ConvoyOrder _ subject1 _ = outValidOrder vconvoy1
            ConvoyOrder _ subject2 _ = outValidOrder vconvoy2
            convoyingAt1 = orderSubjectTarget subject1
            convoyingAt2 = orderSubjectTarget subject2
        in  elem convoyingAt2 (neighbours convoyingAt1)

    initialConvoys :: [([ValidOrder Typical Convoy], Bool)]
    initialConvoys = fmap (\x -> ([x], False)) (filter isInitialConvoy convoys)

    isInitialConvoy :: ValidOrder Typical Convoy -> Bool
    isInitialConvoy vconvoy =
        let ConvoyOrder _ subject _ = outValidOrder vconvoy
            convoyingAt = orderSubjectTarget subject
        in  elem convoyingAt (neighbours movingFrom)

    convoys = withConvoys (extractResolution ctx) (\(vconvoy, res) ->
          let ConvoyOrder _ _ (Convoy _ convoyingFrom convoyingTo) = outValidOrder vconvoy
          in  if movingFrom == convoyingFrom && movingTo == convoyingTo
              then case res of
                Succeeded _ -> Just vconvoy
                Failed _ -> Nothing
              else Nothing
        )

    MoveOrder _ subject (Move movingTo) = outValidOrder validMove

    movingFrom :: ProvinceTarget
    movingFrom = orderSubjectTarget subject

    -- Enumerate (in a probably very inefficient way) all of the non-cyclic
    -- paths (according to an Eq instance) from a set of starting values
    -- according to a generating function and a goal function.
    steps
      :: Eq a
      => [([a], Bool)]
      -- ^ We pair each list with a Bool so we can tell when to stop
      -- expanding it (once the goal function has been satisfied by its
      -- head).
      -> (a -> [a])
      -- ^ Generates a next generation from a seed. Will be applied to the
      -- head of each candidate list which has not yet reached the goal.
      -> (a -> Bool)
      -- ^ The goal function.
      -> [([a], Bool)]
    steps xss next goal = do
      (xs, completed) <- xss
      if completed
      then return (xs, completed)
      else do
        guard $ not (null xs)
        n <- next (head xs)
        guard $ not (elem n xs)
        if goal n
        then return (n : xs, True)
        else steps [(n : xs, False)] next goal

moveOverpowered
  :: ResolutionContext Typical
  -> ValidOrder Typical Move
  -> Resolution Typical Move
moveOverpowered ctx validMoveOrder = overpoweredByHold <|> overpoweredByMove
      -- With this order of Resolutions, we should be able to appeal to 
      -- resolveHold in the latter Resolution: if overpoweredByMove does not
      -- produce a Just, ending the computation, then we know that there is
      -- no dominating move, so we can resolveHold, which will also identify
      -- the lack of a dominating move (doesn't need to look at the
      -- resolutions) and report success!

  where

    overpoweredByHold :: Resolution Typical Move
    overpoweredByHold = case holdingOrders of
      [] -> Nothing
      (vhold, res) : _ -> case res of
        -- It's safe to force res because resolveHold does not force any
        -- resolutions.
        Succeeded _ -> Just (Defended vhold)
        Failed _ -> Nothing

    -- We know (but GHC does not) that there can be at most one element in
    -- this list.
    holdingOrders :: [(ValidOrder Typical Hold, ResolvedOrder Typical Hold)]
    holdingOrders = withHolds (extractResolution ctx) (\(vhold, res) ->
          let HoldOrder _ subject _ = outValidOrder vhold
              holdingAt = orderSubjectTarget subject
          in  if holdingAt == movingTo
              then Just (vhold, res)
              else Nothing
        )

    overpoweredByMove :: Resolution Typical Move
    overpoweredByMove = case mostSupport of
        Nothing -> Just (Standoff (map fst conflictingMoves))
        Just (m, _) ->
          if m == validMoveOrder
          then Nothing
          else Just (MoveOverpowered m)

    mostSupport :: Maybe (ValidOrder Typical Move, ResolvedOrder Typical Move)
    mostSupport = fmap fst (strictMaximum (countSupportMove ctx . fst) conflictingMoves)

    conflictingMoves :: [(ValidOrder Typical Move, ResolvedOrder Typical Move)]
    conflictingMoves = withMoves (extractResolution ctx) (\(vmove, res) ->
          let MoveOrder _ _ (Move movingTo') = outValidOrder vmove
          in  if vmove /= validMoveOrder && movingTo == movingTo'
              then Just (vmove, res)
              else Nothing
        )

    MoveOrder _ _ (Move movingTo) = outValidOrder validMoveOrder

-- | Compute the strict maximum of some list, where each element is associated
--   with an ordered and equality-decidable thing via a function.
--   If there is no strict maximum, you get Nothing.
strictMaximum
  :: (Eq a, Ord a)
  => (b -> a)
  -> [b]
  -> Maybe (b, a)
strictMaximum f = (foldr combine Nothing) . (map (\x -> (x, f x)))
  where
    combine :: (Eq a, Ord a) => (b, a) -> Maybe (b, a) -> Maybe (b, a)
    combine x Nothing = Just x
    combine (x, i) (Just (y, j)) =
        if i == j
        then Nothing
        else if i > j then Just (x, i) else Just (y, j)

-- TODO must revise this one... Must fill in dominator in the where clause
-- to indicate a move from the support's into target which has more power
-- than the support does....
resolveSupport
  :: ResolutionContext Typical
  -> ValidOrder Typical Support
  -> ResolvedOrder Typical Support
resolveSupport ctx validSupportOrder = case dominator of
    Just dominatingMove -> Failed (SupportDislodged dominatingMove)
    Nothing -> case movesWithTarget ctx supportingBase of
      [] -> Succeeded OrderSucceeded
      ms -> Failed (SupportCut (map fst ms))
  where
    supportOrder = outValidOrder validSupportOrder
    supportingBase = orderSubjectTarget (orderSubject supportOrder)
    dominator = undefined -- mostSupportIntoFrom ctx supportingBase (supportingInto supportOrder)

-- Support fails if either 
--   attacked from some place other than supportInto
--   dislodged by move from supportInto

-- | A convoy fails if and only if some move dislodged it.
--
-- Something to think about: does the resolution of a convoy have anything to
-- do with the paradoxes of convoying? I think those are just about the
-- resolutions of supports when a convoy is involved.
-- In that famous Pandin's Paradox, we will find that a convoying fleet is
-- disrupted without being dislodged: an attacker overpowers it, but is also
-- bounced by another equally-supported attacker.
-- So all that we have to do here is check whether
--
--   1. the expected move order was given
--   2. there is _any_ attack on the convoying fleet which _would_ dislodge
--      it, regardless of whether it would actually result in a standoff.
--      So we don't look for a dominator, we look for any attack with strictly
--      more support than the support for the convoying fleet.
--
-- Hm, but we don't want to check other convoying fleets for a path, right?
-- Yes, we consider a convoy to succeed even if the army being convoyed
-- doesn't actually make it, whether due to a standoff at the target or due to
-- an incomplete convoy chain.
--
resolveConvoy
  :: ResolutionContext Typical
  -> ValidOrder Typical Convoy
  -> ResolvedOrder Typical Convoy
resolveConvoy ctx validConvoyOrder = orderResolution resolution

  where

    resolution :: Resolution Typical Convoy
    resolution = expectedMoveOrderNotGiven <|> convoyingFleetOverpowered

    expectedMoveOrderNotGiven :: Resolution Typical Convoy
    expectedMoveOrderNotGiven =
        if null compatibleMove
        then Just ConvoyDeclined
        else Nothing

    -- We know (but GHC doesn't) that this list can be either empty or a
    -- singleton. We use it to decide expectedMoveOrderNotGiven
    compatibleMove :: [ValidOrder Typical Move]
    compatibleMove = withMoves (extractResolution ctx) (\(vord, _) ->
        let MoveOrder _ subject (Move movingTo) = outValidOrder vord
            movingFrom = orderSubjectTarget subject
        in  if   (movingFrom == (convoyingFrom convoyObject))
              && (movingTo == (convoyingTo convoyObject))
            then Just vord
            else Nothing
      )

    ConvoyObject convoyObject = orderObject (outValidOrder validConvoyOrder)

    convoyingFleetOverpowered :: Resolution Typical Convoy
    convoyingFleetOverpowered = undefined

-- Note: the first test for a move which requires a convoy should be a check
-- whether there is at least one convoy chain, because this does not require
-- forcing the resolutions. If that passes, THEN we must force the resolutions
-- of the convoys in each path, until one succeeds (and if none succeeds, order
-- fails).
-- Thus resolveMove will only force a convoy resolution in case that convoy
-- is part of a convoy chain that could carry the move.
-- Here, the check for convoyingFleetOverpowered will stop the move in the
-- case of Pandin's Paradox, because the move which would have cut support is
-- identified as not capable of cutting support _without resolving any convoys_
-- but rather just looking at the ValidOrder values in the resolution:
--   support against a water territory is not cut by a move such that
--   it requires a convoy and its ONLY convoy route which even COULD succeed
--   includes the target of that support. In case there were another convoy
--   route, the support is cut if that convoy route succeeds.
--   What if another convoy route's success depends upon the success of that
--   original convoying fleet (the one being attacked?)
--   Right, we have to cast out every convoy route which includes the attacked
--   convoying fleet! Yes, that's it:
--
--   support against a convoying fleet is NOT cut by a move such that it
--   requires a convoy and EVERY convoy route which even COULD
--   succeed includes the target of that support.

resolveSurrender
  :: ResolutionContext Retreat
  -> ValidOrder Retreat Surrender
  -> ResolvedOrder Retreat Surrender
resolveSurrender _ _ = Succeeded OrderSucceeded

resolveWithdraw
  :: ResolutionContext Retreat
  -> ValidOrder Retreat Withdraw
  -> ResolvedOrder Retreat Withdraw
resolveWithdraw ctx validWithdrawOrder = case conflictingWithdraws ctx validWithdrawOrder of
    [] -> Succeeded OrderSucceeded
    ws -> Failed (WithdrawConflict ws)

resolveDisband
  :: ResolutionContext Adjust
  -> ValidOrder Adjust Disband
  -> ResolvedOrder Adjust Disband
resolveDisband _ _ = Succeeded OrderSucceeded

-- | Note that in order to resolve builds, we must use an ordering of the
--   ValidOrder Adjust Build values in the ResolutionContext Adjust. That's
--   to handle cases in which a country has issued n valid build orders,
--   but has a defecit of -(n - k), and so can build on n - k units. 
--   To resolve a build order in this case, we must treat each valid order
--   slightly differently: some most take precedence over others. We achieve
--   this by using priorBuilds, which uses the order of the list of valid
--   build orders in a ResolvedOrder Adjust.
resolveBuild
  :: ResolutionContext Adjust
  -> ValidOrder Adjust Build
  -> ResolvedOrder Adjust Build
resolveBuild ctx validBuildOrder =
    if currentDefecit >= 0
    then Failed InsufficientSupplyCentres
    else Succeeded OrderSucceeded

  where

    currentDefecit :: Int
    currentDefecit =
      (fixedDefecit ctx issuingCountry) -
      numberOfDisbands +
      numberOfPriorBuilds

    issuingCountry :: Country
    issuingCountry = orderCountry (outValidOrder validBuildOrder)

    numberOfPriorBuilds :: Int
    numberOfPriorBuilds = length (priorBuilds ctx validBuildOrder)

    numberOfDisbands :: Int
    numberOfDisbands = length $ successfulDisbands ctx issuingCountry


successfulDisbands
  :: ResolutionContext Adjust
  -> Country
  -> [OrderSucceeded Adjust Disband]
successfulDisbands ctx country = withDisbands (extractResolution ctx) selector
  where
    selector
      :: (ValidOrder Adjust Disband, ResolvedOrder Adjust Disband)
      -> Maybe (OrderSucceeded Adjust Disband)
    selector (vord, res) =
      let order = outValidOrder vord
          country' = orderCountry order
      in  if country' == country
          then case res of 
            Succeeded s -> Just s
            Failed _ -> Nothing
          else Nothing

-- | Build orders must be ordered; we rely on their ordering to resolve build
--   orders in cases where there are more build orders than would be allowed
--   given that country's defecit.
--   priorBuilds gives a list of ValidOrder Adjust Build such that each of
--   them appears after the one given, in the ResolutionContext, not including
--   the one given.
priorBuilds
  :: ResolutionContext Adjust
  -> ValidOrder Adjust Build
  -> [ValidOrder Adjust Build]
priorBuilds ctx validBuildOrder = fst $ foldr combine ([], False) buildsFromIssuingCountry

  where

    combine _ (xs, True) = (xs, True)
    combine vord (xs, False) =
      if vord == validBuildOrder
      then (xs, True)
      else (vord : xs, False)

    buildsFromIssuingCountry :: [ValidOrder Adjust Build]
    buildsFromIssuingCountry = withBuilds (extractResolution ctx) selector

    issuingCountry = orderCountry (outValidOrder validBuildOrder)

    selector
      :: (ValidOrder Adjust Build, ResolvedOrder Adjust Build)
      -> Maybe (ValidOrder Adjust Build)
    selector (vord, _) =
      let order = outValidOrder vord
          country = orderCountry order
      in if country == issuingCountry then Just vord else Nothing
