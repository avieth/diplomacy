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
--     map :: Diplomacy phase -> DiplomacyMap (PhaseType phase)
--
nextDiplomacy
  :: Diplomacy phase
  -> (ResolvedOrders (PhaseType phase), Diplomacy (NextPhase phase))

nextDiplomacy (DiplomacySpring dmi brd stp) = (res, DiplomacySpringRetreat dmi brd' stp')
  where
    (res, brd') = makeRetreat brd
    stp' = nextStep stp

nextDiplomacy (DiplomacySpringRetreat dmi brd stp) = (res, DiplomacyAutumn dmi brd' stp')
  where
    (res, brd') = makeTypical brd
    stp' = nextStep stp

nextDiplomacy (DiplomacyAutumn dmi brd stp) = (res, DiplomacyAutumnRetreat dmi brd' stp')
  where
    (res, brd') = makeRetreat brd
    stp' = nextStep stp

nextDiplomacy (DiplomacyAutumnRetreat dmi brd stp) = (res, DiplomacyWinter dmi brd' stp')
  where
    (res, brd') = makeAdjust brd
    stp' = nextStep stp

nextDiplomacy (DiplomacyWinter dmi brd stp) = (res, DiplomacySpring dmi brd' stp')
  where
    (res, brd') = endAdjustment brd
    stp' = nextStep stp

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

{-
class DiplomacyPhase phase where
  nextDiplomacy
    :: Orders (PhaseType phase)
    -> Diplomacy phase
    -> Diplomacy (NextPhase phase)

instance DiplomacyPhase Spring where
  nextDiplomacy ords (DiplomacySpring dmi b s) =
    let r = resolveSpring ords b
    in  DiplomacySpringRetreat dmi (nextBoardSpring r) (resolvedOrders r) (nextStep s)

instance DiplomacyPhase SpringRetreat where
  nextDiplomacy ords (DiplomacySpringRetreat dmi b res s) =
    let r = resolveSpringRetreat ords b res
    in  DiplomacyAutumn dmi (nextBoardSpringRetreat r) (nextStep s)

instance DiplomacyPhase Autumn where
  nextDiplomacy ords (DiplomacyAutumn dmi b s) =
    let r = resolveAutumn ords b
    in  DiplomacyAutumnRetreat dmi (nextBoardAutumn r) (resolvedOrders r) (nextStep s)

instance DiplomacyPhase AutumnRetreat where
  nextDiplomacy ords (DiplomacyAutumnRetreat dmi b res s) =
    let r = resolveAutumnRetreat ords b res
    in  DiplomacyWinter dmi (nextBoardAutumnRetreat r) (nextStep s)

instance DiplomacyPhase Winter where
  nextDiplomacy ords (DiplomacyWinter dmi b s) =
    let r = resolveWinter ords b
    in  DiplomacySpring dmi (nextBoardWinter r) (nextStep s)
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
nextBoardSpring :: ResolvedPhase Spring -> Board (PhaseType (NextPhase Spring))
nextBoardSpring (ResolvedPhase (brd, resOrds)) =
    foldr update (makeRetreat brd) resOrds
  where
    -- Type signature is in fact necessary to pass type checking!
    update :: ResolvedOrder Typical -> Board Retreat -> Board Retreat
    update (Invalid _) brd = brd
    update (Failed _) brd = brd
    -- ^ No change for invalid or failed moves.
    --   In Typical phase, only the successful moves induce a change.

    update (Succeeded (MoveSucceeded c os (Move pt))) brd =
      let brd' = occupy (orderSubjectTarget os) Nothing brd
          brd'' = occupy pt (Just $ align (orderSubjectUnit os) c) brd'
      in  maybe brd'' (\u -> dislodge pt (Just u) brd'') (unitAt brd (orderSubjectTarget os))
    -- ^ A successful move puts the mover onto that target territory, and if
    --   there's somebody there, dislodged him. Of course, we never update
    --   control here, that's done at the end of the autumn phase.

    update (Succeeded _) brd = brd
-- ^ No other orders affect any change in the board.

nextBoardSpringRetreat
  :: ResolvedPhase SpringRetreat
  -> Board (PhaseType (NextPhase SpringRetreat))
nextBoardSpringRetreat = undefined

-- Must remember to update control after autumn phase.
nextBoardAutumn :: ResolvedPhase Autumn -> Board (PhaseType (NextPhase Autumn))
nextBoardAutumn (ResolvedPhase (brd, resOrds)) =
    foldr update (makeRetreat brd) resOrds
  where
    update :: ResolvedOrder Typical -> Board Retreat -> Board Retreat
    update (Invalid _) brd = brd
    update (Failed _) brd = brd

    update (Succeeded (HoldSucceeded c os holdOrder)) brd =
      control (ptProvince $ orderSubjectTarget os) (Just c) brd
    -- ^ Successful holds ensure that the country is controlled by the holding
    --   force's country.

    update (Succeeded (MoveSucceeded c os (Move pt))) brd =
      let brd' = occupy (orderSubjectTarget os) Nothing brd
          brd'' = occupy pt (Just $ align (orderSubjectUnit os) c) brd'
          brd''' = control (ptProvince pt) (Just c) brd''
      in  maybe brd''' (\u -> dislodge pt (Just u) brd''') (unitAt brd (orderSubjectTarget os))
    -- ^ A successful hold not only occupies and possibly dislodges, but also
    --   gains control.

    update (Succeeded _) brd = brd

nextBoardAutumnRetreat
  :: ResolvedPhase AutumnRetreat
  -> Board (PhaseType (NextPhase AutumnRetreat))
nextBoardAutumnRetreat = undefined

-- This one is tricky. How do we guarantee that units in excess of supply
-- centre count are removed???
-- We'll have to define some datatype which serves as proof that no country
-- has more units than supply centres. We shall do this in Board.hs
nextBoardWinter
  :: ResolvedPhase Winter
  -> Board (PhaseType (NextPhase Winter))
nextBoardWinter (ResolvedPhase (brd, resOrds)) =
    foldr update (makeTypical brd) resOrds
  where
    update :: ResolvedOrder Adjust -> Board Typical -> Board Typical
    update (Invalid
    update (Succeeded 

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

