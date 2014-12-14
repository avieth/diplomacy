{-# LANGUAGE GADTs #-}

-- Everything you need in order to play diplomacy.

module Diplomacy.Diplomacy (

    Diplomacy

  , diplomacy
  , nextDiplomacy

  , issueOrder
  , checkOrders

  , year
  , step
  , boardMap

  ) where

import Diplomacy.Step
import Diplomacy.Turn
import Diplomacy.Phase
import Diplomacy.Country
import Diplomacy.Board
import Diplomacy.PlayerCount
import Diplomacy.Order
import Diplomacy.OrderInvalid
import Diplomacy.Orders
import Diplomacy.ResolvedOrder
import Diplomacy.ResolvedOrders
import Diplomacy.Resolve
import Diplomacy.Province
import Diplomacy.Unit

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
  where
    info = diplomacyMetaInformation pc

-- | Progression of the diplomacy game is defined by nextDiplomacy, which is
--   a different function depending upon the phase of the game.
--   That's because some phases (retreat phases) require more information to
--   resolve orders (they need the previous phase's order resolution).
--
--   Idea: we have
--     makeRetreat
--     makeTypical
--     makeAdjust
--     normalizeBoard/normalizedBoard
--   and these guys take care of the form of the board and the orders present
--   on it. But what of resolved orders? Who is responsible for taking a set
--   of resolved orders and producing an updated board? To do so, we shall need
--
--     applyResolution :: ResolvedOrders a -> Board a -> Board a
--     
--   hm no let's take a tangent: each Board Typical and Board Retreat has orders
--   in it, so why not have the resolution operate on the board?
--
--     resolveOrdersTypical :: Board Typical -> (Board Retreat, ResolvedOrders Typical)
--     -- resolves orders, carries out resolutions, and gives all the info you
--     -- need.
--
--   What becomes of invalid orders? We still want to allow the user to give
--   invalid orders, as this is part of the game, but we don't want any invalid
--   orders in the board itself. That's OK. The client application can still
--   show the invalid orders even though they are ignored by diplomacy. BUT we
--   need to facilitate the identification of invalid orders, so that the
--   client can show the user that the order is invalid later on, after the
--   phase has passed.
--
--   So: invalid orders are rejected outright by the board.
--   Resolution time involves identifying failed orders only.
--   Yes, I like this.
--
--
--   Note the type signature: we don't need any external information!
--   We'll have
--
--     diplomacy :: PlayerCount -> Diplomacy Spring
--     issueOrder
--       :: Order (PhaseType phase)
--       -> Diplomacy phase
--       -> Either (OrderInvalid (PhaseType phase)) (Diplomacy phase)
--     nextDiplomacy
--       :: Diplomacy phase
--       -> (ResolvedOrders a, Diplomacy (NextPhase phase))
--
--     checkOrders :: Country -> Diplomacy phase -> Orders (PhaseType phase)
--     boardMap :: Diplomacy phase -> DiplomacyMap (PhaseType phase)
--
nextDiplomacy
  :: Diplomacy phase
  -> (ResolvedOrders (PhaseType phase), Diplomacy (NextPhase phase))

nextDiplomacy (DiplomacySpring dmi brd stp) = (res, DiplomacySpringRetreat dmi brd' stp')
  where
    (res, brd') = undefined -- makeRetreat brd
    stp' = nextStep stp

nextDiplomacy (DiplomacySpringRetreat dmi brd stp) = (res, DiplomacyAutumn dmi brd' stp')
  where
    (res, brd') = undefined -- makeTypical brd
    stp' = nextStep stp

nextDiplomacy (DiplomacyAutumn dmi brd stp) = (res, DiplomacyAutumnRetreat dmi brd' stp')
  where
    (res, brd') = undefined -- makeRetreat brd
    stp' = nextStep stp

nextDiplomacy (DiplomacyAutumnRetreat dmi brd stp) = (res, DiplomacyWinter dmi brd' stp')
  where
    (res, brd') = undefined -- makeAdjust brd
    stp' = nextStep stp

nextDiplomacy (DiplomacyWinter dmi brd stp) = (res, DiplomacySpring dmi brd' stp')
  where
    (res, brd') = undefined -- endAdjustment brd
    stp' = nextStep stp

issueOrder
  :: Order (PhaseType phase)
  -> Diplomacy phase
  -> Either (OrderInvalid (PhaseType phase)) (Diplomacy phase)
issueOrder ord dip =
    case giveOrder ord (board dip) of
      Left x -> Left x
      Right brd' -> Right $ alterBoard (const brd') dip

checkOrders :: Country -> Diplomacy phase -> [Order (PhaseType phase)]
checkOrders country = (orders country) . board

boardMap :: Diplomacy phase -> DiplomacyMap (PhaseType phase)
boardMap = diplomacyMap . board

board :: Diplomacy phase -> Board (PhaseType phase)
board (DiplomacySpring _ brd _) = brd
board (DiplomacySpringRetreat _ brd _) = brd
board (DiplomacyAutumn _ brd _) = brd
board (DiplomacyAutumnRetreat _ brd _) = brd
board (DiplomacyWinter _ brd _) = brd

alterBoard
  :: (Board (PhaseType phase) -> Board (PhaseType phase))
  -> Diplomacy phase
  -> Diplomacy phase
alterBoard f (DiplomacySpring x brd y) = DiplomacySpring x (f brd) y
alterBoard f (DiplomacySpringRetreat x brd y) = DiplomacySpringRetreat x (f brd) y
alterBoard f (DiplomacyAutumn x brd y) = DiplomacyAutumn x (f brd) y
alterBoard f (DiplomacyAutumnRetreat x brd y) = DiplomacyAutumnRetreat x (f brd) y
alterBoard f (DiplomacyWinter x brd y) = DiplomacyWinter x (f brd) y

step :: Diplomacy a -> Step a
step (DiplomacySpring _ _ s) = s
step (DiplomacySpringRetreat _ _ s) = s
step (DiplomacyAutumn _ _ s) = s
step (DiplomacyAutumnRetreat _ _ s) = s
step (DiplomacyWinter _ _ s) = s

year :: Diplomacy a -> Int
year d = let currentTurn = turn (step d) in (asYear (startingYear d)) currentTurn

-- | TODO when supporting two player mode, starting year is 1914.
startingYear :: Diplomacy a -> Year
startingYear d = 1901
