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

data OrderSucceeded phaseType orderType where
  OrderSucceeded :: OrderSucceeded phaseType orderType

data OrderFailed phaseType orderType where

  HoldOverpowered
    :: OrderSucceeded Typical Move
    -- ^ The move which overpowered the hold.
    -> OrderFailed Typical Hold

  Standoff
    :: [Order Typical Move]
    -> OrderFailed Typical Move

  WouldDislodgeOwnUnit
    :: OrderFailed Typical Move

  SupportCut
    :: [ValidOrder Typical Move]
    -> OrderFailed Typical Support

  SupportDislodged
    :: OrderSucceeded Typical Move
    -> OrderFailed Typical Support

  ConvoyDislodged
    :: OrderSucceeded Typical Move
    -> OrderFailed Typical Convoy

  -- Failed because either the expected move order wasn't placed, or because
  -- other expected convoy orders were not placed.
  ConvoyFailed
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

holdSupport
  :: ResolutionContext Typical
  -> ValidOrder Typical Hold
  -> [(ValidOrder Typical Support, OrderSucceeded Typical Support)]
holdSupport ctx validHoldOrder = withSupports (extractResolution ctx) selector
  where
    selector (vsupport, res) =
      let SupportObject s = orderObject (outValidOrder vsupport)
          supportFrom = supportingFrom s
          supportInto = supportingInto s
          holdOrder = outValidOrder validHoldOrder
          holdingAt = orderSubjectTarget (orderSubject holdOrder)
      in  if supportFrom == supportInto && supportFrom == holdingAt
          then case res of
            Succeeded s -> Just (vsupport, s)
            Failed _ -> Nothing
          else Nothing

conflictingWithdraws
  :: ResolutionContext Retreat
  -> ValidOrder Retreat Withdraw
  -> [ValidOrder Retreat Withdraw]
conflictingWithdraws validWithdraw = undefined

-- TODO shouldn't this be called MoveSupport and take a ValidOrder Typical Move?
supportsFromInto
  :: ResolutionContext Typical
  -> ProvinceTarget
  -> ProvinceTarget
  -> [(ValidOrder Typical Support, OrderSucceeded Typical Support)]
supportsFromInto ctx ptFrom ptTo = withSupports (extractResolution ctx) selector
  where
    selector (vsupport, res) =
      let SupportObject s = orderObject (outValidOrder vsupport)
          supportFrom = supportingFrom s
          supportInto = supportingInto s
      in  if supportFrom == ptFrom && supportInto == ptTo
          -- Careful only to force res in case the above condition holds.
          then case res of
            Succeeded s -> Just (vsupport, s)
            Failed _ -> Nothing
          else Nothing

-- TODO need this? Delete if nobody uses it.
supportsInto
  :: ResolutionContext Typical
  -> ProvinceTarget
  -> [(ValidOrder Typical Support, OrderSucceeded Typical Support)]
supportsInto ctx ptTo = withSupports (extractResolution ctx) selector
  where
    selector (vsupport, res) =
      let SupportObject s = orderObject (outValidOrder vsupport)
          supportInto = supportingInto s
      in  if supportInto == ptTo
          then case res of
            Succeeded s -> Just (vsupport, s)
            Failed _ -> Nothing
          else Nothing

-- | Determine the move order with (strictly) the most support into a given
--   ProvinceTarget, or Nothing in case there is none such. To rephrase: of
--   all the ValidOrder Typical Move values in the ResolutionContext Typical,
--   find the one which has strictly more support than any other.
mostSupportInto
  :: ResolutionContext Typical
  -> ProvinceTarget
  -> Maybe (ValidOrder Typical Move, Int)
mostSupportInto ctx provinceTarget = dominator movesWithSupport

  where

    dominator :: [(ValidOrder Typical Move, Int)] -> Maybe (ValidOrder Typical Move, Int)
    dominator = foldr combine Nothing
      where
        -- We track the greatest seen so far, but we dump it in case we find
        -- another which is equal. In this way, we always end up with the
        -- strict maximum, or Nothing if there is no strict maximum.
        combine (v, i) Nothing = Just (v, i)
        combine (v, i) (Just (v', j)) =
          if i == j
          then Nothing
          else if i > j then Just (v, i) else Just (v', j)

    movesWithSupport :: [(ValidOrder Typical Move, Int)]
    movesWithSupport = withMoves (extractResolution ctx) countSupport

    countSupport
      :: (ValidOrder Typical Move, ResolvedOrder Typical Move)
      -> Maybe (ValidOrder Typical Move, Int)
    countSupport (validMove, _) =
      let MoveObject m = orderObject (outValidOrder validMove)
          subject = orderSubject (outValidOrder validMove)
          movingInto = moveTarget m
          movingFrom = orderSubjectTarget subject
      in  if movingInto == provinceTarget
          then Just (validMove, length $ supportsFromInto ctx movingFrom movingInto)
          else Nothing

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
--   succeeds. This is not a circular definition, because the resolution of
--   moves does not depend upon the resolution of any holds.
resolveHold
  :: ResolutionContext Typical
  -> ValidOrder Typical Hold
  -> ResolvedOrder Typical Hold
resolveHold ctx validHoldOrder = case withMoves (extractResolution ctx) selector of
    [] -> Succeeded (OrderSucceeded)
    [s] -> Failed (HoldOverpowered s)
    -- If we reach this case, it means more than one move into a particular
    -- province target has succeeded, but that's not possible by definition
    -- of resolveMove
    _ -> error "Impossible"
  where
    selector (vmove, res) =
      let MoveObject m = orderObject (outValidOrder vmove)
          movingInto = moveTarget m
          holdOrder = outValidOrder validHoldOrder
          holdingAt = orderSubjectTarget (orderSubject holdOrder)
      in  if movingInto == holdingAt
          then case res of
            Succeeded s -> Just s
            Failed _ -> Nothing
          else Nothing

-- This will be the hardest one I think.
resolveMove
  :: ResolutionContext Typical
  -> ValidOrder Typical Move
  -> ResolvedOrder Typical Move
resolveMove ctx validMoveOrder =
    if False --isConvoyed validMoveOrder
    then resolveConvoyedMove ctx validMoveOrder
    else resolveNormalMove ctx validMoveOrder

resolveConvoyedMove
  :: ResolutionContext Typical
  -> ValidOrder Typical Move
  -> ResolvedOrder Typical Move
resolveConvoyedMove ctx validMoveOrder = undefined

resolveNormalMove
  :: ResolutionContext Typical
  -> ValidOrder Typical Move
  -> ResolvedOrder Typical Move
resolveNormalMove ctx validMoveOrder = undefined

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



-- A convoy fails if and only if some move dislodged it.
resolveConvoy
  :: ResolutionContext Typical
  -> ValidOrder Typical Convoy
  -> ResolvedOrder Typical Convoy
resolveConvoy ctx validConvoyOrder = undefined

{-case dominator of
    Just dominatingMove -> Failed (ConvoyDislodged dominatingMove)
    Nothing -> Succeeded
  where
    convoyOrder = outValidOrder validConvoyOrder
    convoyingTarget = orderSubjectTarget (orderSubject convoyOrder)
    dominator = mostSupportInto ctx convoyingTarget
    -}

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

-- Not so easy! What if there are n disbands, n+1 builds, and defecit is 0?
-- All builds but one should succeed. Can we express this even with out
-- recursive-let style? I think it will induce a cycle and nontermination!!!
-- Each build will wait upon the other builds!
-- Solution: order the builds? Eliminate defecit excess as invalid?
-- Yes, ordering will work. Must define
--   getPriorBuilds :: ResolutionContext Adjust -> Order Adjsut Build -> [Order Adjust Build]
-- and use only prior builds and disbands in computing the defecit.
-- Prior will mean "issued before"
resolveBuild
  :: ResolutionContext Adjust
  -> ValidOrder Adjust Build
  -> ResolvedOrder Adjust Build
resolveBuild ctx validBuildOrder =
    if currentDefecit >= 0
    then Failed InsufficientSupplyCentres
    else Succeeded OrderSucceeded
  where
    currentDefecit = undefined
    {-
    currentDefecit = (fixedDefecit ctx country) - (numberOfDisbands ctx country) + numberOfPriorBuilds
    numberOfPriorBuilds = length (priorBuilds ctx validBuildOrder)
    country = orderSubjectCountry (orderSubject ((outValidOrder validBuildOrder)))
    -}

holdOrders = undefined
moveOrders = undefined
supportOrders = undefined
convoyOrders = undefined
surrenderOrders = undefined
withdrawOrders = undefined
disbandOrders = undefined
buildOrders = undefined
