module Diplomacy.Step (

    Step

  -- Projections from Step
  , turn
  , phase

  -- Injection into Step
  , makeStep

  , nextStep

  ) where

import Diplomacy.Turn
import Diplomacy.Phase

-- | A point of play in a game of diplomacy.
newtype Step a = Step (Turn, Phase a)
  deriving (Eq, Ord, Show)

turn :: Step a -> Turn
turn (Step (t, _)) = t

phase :: Step a -> Phase a
phase (Step (_, p)) = p

makeStep :: Turn -> Phase a -> Step a
makeStep t p = Step (t, p)

nextStep :: Step a -> Step (NextPhase a)
nextStep st = makeStep theNextTurn (nextPhase (phase st))
  where theNextTurn = if isWinter (phase st) then nextTurn (turn st) else (turn st)
