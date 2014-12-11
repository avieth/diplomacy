{-# LANGUAGE GADTs #-}

-- Everything you need in order to play diplomacy.

module Diplomacy.Diplomacy where

import Diplomacy.Step
import Diplomacy.Turn
import Diplomacy.Phase
import Diplomacy.Board
import Diplomacy.PlayerCount
import Diplomacy.Orders

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

-- TODO import this?
data ResolvedOrders phaseType

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

resolveSpring
  :: Orders (PhaseType Spring)
  -> Board (PhaseType Spring)
  -> ResolvedPhase Spring
resolveSpring = undefined

resolveSpringRetreat
  :: Orders (PhaseType SpringRetreat)
  -> Board (PhaseType SpringRetreat)
  -> ResolvedOrders (PhaseType Spring)
  -- ^ Gotta have those orders from the previous Spring phase.
  -> ResolvedPhase SpringRetreat
resolveSpringRetreat = undefined

resolveAutumn
  :: Orders (PhaseType Autumn)
  -> Board (PhaseType Autumn)
  -> ResolvedPhase Autumn
resolveAutumn= undefined

resolveAutumnRetreat
  :: Orders (PhaseType AutumnRetreat)
  -> Board (PhaseType AutumnRetreat)
  -> ResolvedOrders (PhaseType Autumn)
  -> ResolvedPhase AutumnRetreat
resolveAutumnRetreat = undefined

resolveWinter
  :: Orders (PhaseType Winter)
  -> Board (PhaseType Winter)
  -> ResolvedPhase Winter
resolveWinter = undefined

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
