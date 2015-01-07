{-# LANGUAGE GADTs #-}

module Diplomacy.OrderInvalid (

    OrderInvalid(..)

  , validateOrder

  ) where

import Control.Applicative

import Diplomacy.Phase
import Diplomacy.Province
import Diplomacy.Unit
import Diplomacy.Order
import Diplomacy.Country
import Diplomacy.Board
import Diplomacy.ResolvedOrder

-- | Any order can be invalid. Each witness gives a reason.
--   The type parameter indicates the phase to which it is relevant.
data OrderInvalid phaseType where

  -- | Aligned unit not present at given province target.
  --   Applies for any phase.
  SubjectInvalid :: OrderInvalid phaseType

  -- | Applies to all phases: move, withdraw, and build
  --   Army cannot go in water, fleet cannot go inland, whether at the request
  --   of a move, withdraw, or build.
  UnitCannotGoHere :: OrderInvalid phaseType

  -- | Unit cannot move from one ProvinceTarget to another ProvinceTarget.
  --   Applies only for the Typical phase type. There is another constructor
  --   for an invalid move in the retreat phase type.
  NotReachable :: OrderInvalid Typical

  -- | Supported unit is not there?
  SupportedUnitNotPresent :: OrderInvalid Typical

  -- | Supporting an attack against one's own unit is not valid.
  SupportAgainstSelf :: OrderInvalid Typical

  FleetCannotConvoy :: OrderInvalid Typical

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
  NotConvoyAdjacent :: OrderInvalid Typical

  -- | A non-dislodged unit cannot surrender nor withdraw.
  UnitNotDislodged :: OrderInvalid Retreat

  -- | A unit cannot withdraw into an attacking province.
  WithdrawIntoAttackingProvince :: OrderInvalid Retreat

  -- | A unit cannot withdraw into a non-adjacent country.
  --   Distinct from NotReachable, which only arises in Typical phases.
  WithdrawTargetNotAdjacent :: OrderInvalid Retreat

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
  IsNotHomeSupplyCentre :: OrderInvalid Adjust

-- | A newtype to distinguish Orders which have been validated.
--   Would be cool if we could associate them with a Board in the type, but
--   that would demand some dependent type action.
newtype ValidOrder phaseType = ValidOrder {
    validOrder :: Order phaseType
  }

type OrderValidation phaseType = Either (OrderInvalid phaseType) (ValidOrder phaseType)

type Validation phaseType = Maybe (OrderInvalid phaseType)

orderValidation :: Order phaseType -> Validation phaseType -> OrderValidation phaseType
orderValidation _ (Just x) = Left $ x
orderValidation x Nothing = Right $ ValidOrder x

ok :: Validation phaseType
ok = Nothing

notOk :: OrderInvalid phaseType -> Validation phaseType
notOk = Just

-- TBD these guys really helpful? Maybe get rid of them.
type Validator phaseType = Board phaseType -> Order phaseType -> Validation phaseType
also :: Validator phaseType -> Validator phaseType -> Validator phaseType
also check1 check2 board order = check1 board order <|> check2 board order

orElse :: Bool -> OrderInvalid phaseType -> Validation phaseType
orElse True _ = ok
orElse False x = notOk x

implies :: Bool -> OrderInvalid phaseType -> Validation phaseType
implies True x = notOk x
implies False _ = ok

validateOrder :: Board phaseType -> Order phaseType -> OrderValidation phaseType
validateOrder board order = orderValidation order validation
  where
    validation = (validateSubject board order) <|> (validateObject board order)

validateSubject :: Board phaseType -> Order phaseType -> Validation phaseType
validateSubject board order = case order of
  -- TODO for non build orders we must ensure that the Unit specified by the
  -- subject occupies the ProvinceTarget specified by that subject.
  --
  -- NB for the Retreat phase this is about checking the DISLODGED map, not
  -- the OCCUPY map, but for Typical we check the OCCUPY map and for Adjust
  -- (the Disband order) we also check OCCUPY.
  HoldOrder country subject _ -> validateSubjectTypical board country subject
  MoveOrder country subject _ -> validateSubjectTypical board country subject
  SupportOrder country subject _ -> validateSubjectTypical board country subject
  ConvoyOrder country subject _ -> validateSubjectTypical board country subject
  SurrenderOrder country subject _ -> validateSubjectRetreat board country subject
  WithdrawOrder country subject _ -> validateSubjectRetreat board country subject
  DisbandOrder country subject _ -> validateSubjectAdjust board country subject
  BuildOrder country subject object -> ok


validateSubjectTypical :: Board Typical -> Country -> OrderSubject -> Validation Typical
validateSubjectTypical board country subject = condition `orElse` reason
  where
    condition = occupies au provinceTarget board
    reason = SubjectInvalid
    au = align unit country
    unit = orderSubjectUnit subject
    provinceTarget = orderSubjectTarget subject

validateSubjectRetreat :: Board Retreat -> Country -> OrderSubject -> Validation Retreat
validateSubjectRetreat board country subject = condition `orElse` reason
  where
    condition = isDislodged board au provinceTarget
    reason = SubjectInvalid
    au = align unit country
    unit = orderSubjectUnit subject
    provinceTarget = orderSubjectTarget subject

validateSubjectAdjust :: Board Adjust -> Country -> OrderSubject -> Validation Adjust
validateSubjectAdjust board country subject = condition `orElse` reason
  where
    condition = occupies au provinceTarget board
    reason = SubjectInvalid
    au = align unit country
    unit = orderSubjectUnit subject
    provinceTarget = orderSubjectTarget subject

validateObject :: Board phaseType -> Order phaseType -> Validation phaseType
validateObject board order = case order of
  HoldOrder country subject object -> validateHold board country subject object
  MoveOrder country subject object -> validateMove board country subject object
  SupportOrder country subject object -> validateSupport board country subject object
  ConvoyOrder country subject object -> validateConvoy board country subject object
  SurrenderOrder country subject object -> validateSurrender board country subject object
  WithdrawOrder country subject object -> validateWithdraw board country subject object
  DisbandOrder country subject object -> validateDisband board country subject object
  BuildOrder country subject object -> validateBuild board country subject object

validateHold :: Board Typical -> Country -> OrderSubject -> Hold -> Validation Typical
validateHold board country subject Hold = ok

validateMove :: Board Typical -> Country -> OrderSubject -> Move -> Validation Typical
validateMove board country subject (Move provinceTarget) =
        unitCannotOccupy unit provinceTarget
    <|> cannotMoveFromTo unit fromPt provinceTarget
  where
    unit = orderSubjectUnit subject
    fromPt = orderSubjectTarget subject

validateSupport :: Board Typical -> Country -> OrderSubject -> Support -> Validation Typical
validateSupport board country subject (Support unit provinceTarget maybeProvinceTarget) =
        (unitOccupies unit provinceTarget board `orElse` SupportedUnitNotPresent)
    <|> noSupportAgainstSelf
  where
    pt' = orderSubjectTarget subject
    noSupportAgainstSelf = case maybeProvinceTarget of
      Nothing -> ok
      Just pt -> (not (countryOccupies country pt board)) `orElse` SupportAgainstSelf

validateConvoy :: Board Typical -> Country -> OrderSubject -> Convoy -> Validation Typical
validateConvoy board country subject (Convoy unit ptFrom ptTo) =
        isArmy unit `orElse` FleetCannotConvoy
    <|> convoyAdjacent ptFrom ptTo `orElse` NotConvoyAdjacent
  where
    ptSource = orderSubjectTarget subject

validateSurrender :: Board Retreat -> Country -> OrderSubject -> Surrender -> Validation Retreat
validateSurrender board _ subject _ = unitNotDislodged board subject

validateWithdraw :: Board Retreat -> Country -> OrderSubject -> Withdraw -> Validation Retreat
validateWithdraw board _ subject (Withdraw toProvinceTarget) =
        unitNotDislodged board subject
    <|> notWithdrawAdjacent subject toProvinceTarget
    <|> withdrawingIntoAttackingProvince board subject toProvinceTarget

validateDisband :: Board Adjust -> Country -> OrderSubject -> Disband -> Validation Adjust
validateDisband board country subject Disband = ok

validateBuild :: Board Adjust -> Country -> OrderSubject -> Build -> Validation Adjust
validateBuild board country subject Build = 
        unitCannotOccupy unit provinceTarget
    <|> notInHomeSupplyCentre country provinceTarget
  where
    unit = orderSubjectUnit subject
    provinceTarget = orderSubjectTarget subject

notInHomeSupplyCentre :: Country -> ProvinceTarget -> Validation Adjust
notInHomeSupplyCentre country provinceTarget = condition `orElse` reason
  where
    province = ptProvince provinceTarget
    isSupplyCentre = supplyCentre province
    isHomeProvince = isHome country province
    condition = isHomeProvince && isSupplyCentre
    reason = IsNotHomeSupplyCentre

unitCannotOccupy :: Unit -> ProvinceTarget -> Validation phaseType
unitCannotOccupy u pt = condition `implies` reason
  where
    condition =  (isArmy u && isWater (ptProvince pt))
              || (isFleet u && isInland (ptProvince pt))
              || (isArmy u && isSpecial pt)
    reason = UnitCannotGoHere

cannotMoveFromTo :: Unit -> ProvinceTarget -> ProvinceTarget -> Validation Typical
cannotMoveFromTo unit fromPt toPt = condition `implies` reason
  where
    reason = NotReachable
    condition = if isArmy unit
                then moveAdjacent fromPt toPt || convoyAdjacent fromPt toPt
                else moveAdjacent fromPt toPt

-- Unlike move adjacency, here we do not admit convoy adjacency.
notWithdrawAdjacent :: OrderSubject -> ProvinceTarget -> Validation Retreat
notWithdrawAdjacent subject toPt = condition `orElse` reason
  where
    unit = orderSubjectUnit subject
    fromPt = orderSubjectTarget subject
    reason = WithdrawTargetNotAdjacent
    condition = moveAdjacent fromPt toPt

-- | We assume the OrderSubject's unit is consistent with the unit on the
--   board, i.e. that the OrderSubject has passed subject validation.
unitNotDislodged :: Board Retreat -> OrderSubject -> Validation Retreat
unitNotDislodged board subject = condition `orElse` reason
  where
    provinceTarget = orderSubjectTarget subject
    unit = orderSubjectUnit subject
    -- No need for isDislodged; see function comment.
    condition = hasDislodged board provinceTarget
    reason = UnitNotDislodged

withdrawingIntoAttackingProvince :: Board Retreat -> OrderSubject -> ProvinceTarget -> Validation Retreat
withdrawingIntoAttackingProvince board subject toProvinceTarget =
    condition `implies` reason
  where
    reason = WithdrawIntoAttackingProvince
    condition = anyResolvedOrder board predicate
    predicate resolvedOrder = case resolvedOrder of
      Succeeded (MoveSucceeded _ _ (Move toProvinceTarget')) ->
        toProvinceTarget' == toProvinceTarget
      _ -> False
    -- implementing this guy forces us to think about the ResolvedOrder datatype,
    -- and to define the interface through Board Retreat with which we can
    -- check the ResolvedOrders. We'll also need predicates on ResolvedOrder so
    -- that we can tell whether its a move order, and what is it's target.
