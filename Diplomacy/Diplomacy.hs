{-# LANGUAGE GADTs #-}

-- Everything you need in order to play diplomacy.

module Diplomacy.Diplomacy where

import Diplomacy.Step
import Diplomacy.Turn
import Diplomacy.Phase
import Diplomacy.Board
import Diplomacy.PlayerCount
import Diplomacy.Order
import Diplomacy.Orders
import Diplomacy.ResolvedOrder
import Diplomacy.ResolvedOrders
import Diplomacy.Resolve

-- | Information about a game of diplomacy which will not change throughout
--   the course of the game.
data DiplomacyMetaInformation = DiplomacyMetaInformation {
    playerCount :: PlayerCount
  }

diplomacyMetaInformation :: PlayerCount -> DiplomacyMetaInformation
diplomacyMetaInformation = DiplomacyMetaInformation

data Diplomacy phase where

  DiplomacySpring
    :: DiplomacyMetaInformation
    -> Board (PhaseType Spring)
    -> Step Spring
    -> Diplomacy Spring

  DiplomacySpringRetreat
    :: DiplomacyMetaInformation
    -> Board (PhaseType SpringRetreat)
    -> ResolvedOrders (PhaseType Spring)
    -> Step SpringRetreat
    -> Diplomacy SpringRetreat

  DiplomacyAutumn
    :: DiplomacyMetaInformation
    -> Board (PhaseType Autumn)
    -> Step Autumn
    -> Diplomacy Autumn

  DiplomacyAutumnRetreat
    :: DiplomacyMetaInformation
    -> Board (PhaseType AutumnRetreat)
    -> ResolvedOrders (PhaseType Autumn)
    -> Step AutumnRetreat
    -> Diplomacy AutumnRetreat

  DiplomacyWinter
    :: DiplomacyMetaInformation
    -> Board (PhaseType Winter)
    -> Step Winter
    -> Diplomacy Winter

-- | A new game of diplomacy.
--   Starts in spring, with the initial board.
diplomacy :: PlayerCount -> Diplomacy Spring
diplomacy pc = DiplomacySpring info (initialBoard pc) (makeStep firstTurn springPhase)
  where info = diplomacyMetaInformation pc

-- | Progression of the diplomacy game is defined by nextDiplomacy, which is
--   a different function depending upon the phase of the game.
--   That's because some phases (retreat phases) require more information to
--   resolve orders (they need the previous phase's order resolution).
class DiplomacyPhase phase where
  nextDiplomacy
    :: Orders (PhaseType phase)
    -> Diplomacy phase
    -> Diplomacy (NextPhase phase)

instance DiplomacyPhase Spring where
  nextDiplomacy ords (DiplomacySpring dmi b s) =
    let r = resolveSpring ords b
    in  DiplomacySpringRetreat dmi (nextBoard r) (resolvedOrders r) (nextStep s)

instance DiplomacyPhase SpringRetreat where
  nextDiplomacy ords (DiplomacySpringRetreat dmi b res s) =
    let r = resolveSpringRetreat ords b res
    in  DiplomacyAutumn dmi (nextBoard r) (nextStep s)

instance DiplomacyPhase Autumn where
  nextDiplomacy ords (DiplomacyAutumn dmi b s) =
    let r = resolveAutumn ords b
    in  DiplomacyAutumnRetreat dmi (nextBoard r) (resolvedOrders r) (nextStep s)

instance DiplomacyPhase AutumnRetreat where
  nextDiplomacy ords (DiplomacyAutumnRetreat dmi b res s) =
    let r = resolveAutumnRetreat ords b res
    in  DiplomacyWinter dmi (nextBoard r) (nextStep s)

instance DiplomacyPhase Winter where
  nextDiplomacy ords (DiplomacyWinter dmi b s) =
    let r = resolveWinter ords b
    in  DiplomacySpring dmi (nextBoard r) (nextStep s)

-- TBD import this?
-- With the order type in the picture, we'll have to use type classes with
-- an existential type to express.
--updateBoard (MoveSucceeded move) = -- Need to know entire order (with subject).
--updateBoard _ = id
{-
newtype ResolvedOrders phaseType
  = ResolvedOrders [ResolvedOrder phaseType]

-- | Take the subset of ResolvedOrders which succeeded.
succeeded :: ResolvedOrders phaseType -> [OrderSucceeded phaseType]
succeeded (ResolvedOrders xs) = filter orderSucceeded xs

failed :: ResolvedOrders phaseType -> [OrderFailed phaseType]
failed (ResolvedOrders xs) = filter orderFailed xs

updateBoard :: OrderSucceeded phaseType -> Board phaseType -> Board phaseType
updateBoard = undefined
-}

-- | Values in this type pair a board with resolved orders which were resolved
--   against that board.
newtype ResolvedPhase phase
  = ResolvedPhase (Board (PhaseType phase), ResolvedOrders (PhaseType phase))

-- | Use the information in a ResolvedPhase to produce the next board.
--   That's to say, take all of the successful orders and execute them.
--
--   Typical phase type orders:
--     Successful moves transplant units.
--
--   Retreat phase type orders:
--     Successful retreats transplant units.
--     Failed withdraws remove units.
--     Successful surrenders remove units.
--
--   Adjust phase type orders:
--     Successful disbands remove units.
--     Successful builds add units.
--
-- Should express this one as a fold on the ResolvedOrders value, starting
-- with the current board and altering it for each of the above cases.
nextBoard :: ResolvedPhase a -> Board (PhaseType (NextPhase a))
nextBoard = undefined

resolvedOrders :: ResolvedPhase a -> ResolvedOrders (PhaseType a)
resolvedOrders (ResolvedPhase (_, resOrds)) = resOrds

-- TODO move these to ResolvedOrder.hs ?
resolveSpring
  :: Orders (PhaseType Spring)
  -> Board (PhaseType Spring)
  -> ResolvedPhase Spring
resolveSpring ords brd
  = ResolvedPhase (brd, map (resolveOrderTypical ords brd) ords)

resolveSpringRetreat
  :: Orders (PhaseType SpringRetreat)
  -> Board (PhaseType SpringRetreat)
  -> ResolvedOrders (PhaseType Spring)
  -- ^ Gotta have those orders from the previous Spring phase.
  -> ResolvedPhase SpringRetreat
resolveSpringRetreat ords brd res
  = ResolvedPhase (brd, map (resolveOrderRetreat ords brd res) ords)

resolveAutumn
  :: Orders (PhaseType Autumn)
  -> Board (PhaseType Autumn)
  -> ResolvedPhase Autumn
resolveAutumn ords brd
  = ResolvedPhase (brd, map (resolveOrderTypical ords brd) ords)

resolveAutumnRetreat
  :: Orders (PhaseType AutumnRetreat)
  -> Board (PhaseType AutumnRetreat)
  -> ResolvedOrders (PhaseType Autumn)
  -> ResolvedPhase AutumnRetreat
resolveAutumnRetreat ords brd res
  = ResolvedPhase (brd, map (resolveOrderRetreat ords brd res) ords)

resolveWinter
  :: Orders (PhaseType Winter)
  -> Board (PhaseType Winter)
  -> ResolvedPhase Winter
resolveWinter ords brd
  = ResolvedPhase (brd, map (resolveOrderAdjust brd) ords)

step :: Diplomacy a -> Step a
step (DiplomacySpring _ _ s) = s
step (DiplomacySpringRetreat _ _ _ s) = s
step (DiplomacyAutumn _ _ s) = s
step (DiplomacyAutumnRetreat _ _ _ s) = s
step (DiplomacyWinter _ _ s) = s

year :: Diplomacy a -> Int
year d = let currentTurn = turn (step d) in (asYear (startingYear d)) currentTurn

-- | TODO when supporting two player mode, starting year is 1914.
startingYear :: Diplomacy a -> Year
startingYear d = 1901
